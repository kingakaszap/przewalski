# final code for diss
# 2024 march - april

library (tidyverse)
library(readxl)
library(writexl)
library (lme4)
library(MuMIn)
library(ggeffects)
library(effects)
library(sjPlot)   
library(insight)
library(httr)
library(knitr)
library(kableExtra)
library(fortunes)

# import data----
# group composition datasets
total <-read_xlsx("przewalski/data/groups/long/full_clean.xlsx") 
older_data <- read_xlsx("przewalski/data/groups/1997_2008.xlsx") 
older_data <- older_data %>% 
  select (-comment) %>% 
  filter (harem != "enter" &
            harem != "escaped" &
            harem != "export" &
            harem != "uk")
total_1997<- rbind (older_data, total)
total <- total_1997
# data on individual life history events of horses
nagylista <- read_excel("przewalski/data/nagylista.xls", 
                        col_types = c("text", "text", "text", 
                                      "text", "date", "text", "text", "text", 
                                      "text", "text", "text", "text", "date", 
                                      "text", "date", "text", "text", "text", 
                                      "text", "text", "text", "text", "text"))

# creating explanatory and response variables of interest ----

# 1 - time since last observation

total$date<-as.Date(total$date)
total<- total %>% 
  group_by(name) %>% 
  # dataset is already ordered by date
  mutate (last_obs = as.Date(lag(date)) ) %>% # last time this horse appears in dataset
  mutate(time_to_last_obs = as.numeric(date - last_obs)) %>% # dif in days
  ungroup()

# 2,3 -  harem on previous observation ->  change/not (response var)

total <- total %>% 
  arrange(date) %>%
  group_by(name) %>%
  mutate(previous_harem = lag(harem)) %>% # harem on previous observation
  ungroup() %>% 
  group_by (date, name) %>% 
  mutate(change = case_when(
    harem == previous_harem ~ 0, # no change in harem affilation
    harem != previous_harem ~ 1, # harem affilation changed since last observation
    is.na(previous_harem) ~ NA)) %>% 
  ungroup() 

# 4 - unique life history traits (join datasets)

nagylista$name<- as.character(nagylista$Name)
total_full <-dplyr::left_join(total, nagylista, by = c("name"))
# removing variables i don't need
total_full <- total_full %>%  select(-last_obs, -N, -St.b.Num., -Name, -Place_of_birth,  - Num_father, -Num_mother,-Number_of_Davis_case, -Branding, -Origin, 
                                     -`Known_date_(YMD)`, -`Natural(N)_or_Shoot(S)`,  -Chip, -Leavingdate, -Destination, - PZPtreatment_dateofprimer, -Note, -...23)

# 5 - age of horse & year of observation 

total_full <- total_full %>% 
  mutate_if(is.POSIXct, as.Date) %>% # making sure dates are read correctly
  mutate(year_observation = as.numeric(format(as.Date(date, format = "%Y-%m-%d"), "%Y")),
         age = as.numeric(date- Date_of_birth)) 

# assigning foals to their mothers for exp. variable with_foal
# born this year, previous year, and next year
# foals born in year of observation
foals<- select(nagylista, Name, Date_of_birth, Name_mother, Name_father, Date_of_death) 
foals<- foals %>% 
  mutate(name = as.character(Name_mother),
         name_foal = as.character(Name),
         father_of_foal = as.character(Name_father),
         dob_foal = as.Date(Date_of_birth),
         death_foal = as.Date(Date_of_death),
         year_birth = format(as.Date(dob_foal, format = "%Y-%m-%d"), "%Y") ,
         year_observation = as.numeric(year_birth)) %>% 
  select (-year_birth, -Name_mother, -Name, -Date_of_birth, -Name_father, - Date_of_death)
total_foals <- left_join(total_full, foals, by = c("name", "year_observation"))

# foals born in year before observation
foals_before <- select(nagylista, Name, Date_of_birth, Name_mother, Name_father, Date_of_death)
foals_before <- foals_before %>% 
  mutate(name = as.character(Name_mother),
         foal_prev_year = as.character(Name),
         father_of_foal_prev_year = as.character(Name_father),
         dob_foal_prev_year = as.Date(Date_of_birth),
         death_foal_prev_year = as.Date(Date_of_death),
         year_birth = as.numeric(format(as.Date(dob_foal_prev_year, format = "%Y-%m-%d"), "%Y")),
         year_observation = as.numeric(year_birth +1)) %>% 
  select (-year_birth, -Name_mother, -Name, -Date_of_birth, -Name_father, - Date_of_death)
# this will match the year foal is born in to observations in the next year.
# e.g. Foal is born in 2008 - will show at mother's 2009 observation.
total_foals <- left_join (total_foals, foals_before, by = c("name", "year_observation"))

# foals born in year after the observation year
foals_nextyear <- select(nagylista, Name,  Date_of_birth, Name_mother, Name_father, Date_of_death)
foals_nextyear <- foals_nextyear %>% 
  mutate(name = as.character(Name_mother),
         foal_next_year = as.character(Name),
         father_of_foal_next_year = as.character(Name_father),
         dob_foal_next_year = as.Date(Date_of_birth),
         death_foal_next_year = as.Date(Date_of_death),
         year_birth = as.numeric(format(as.Date(dob_foal_next_year, format = "%Y-%m-%d"), "%Y")),
         year_observation = as.numeric(year_birth - 1)) %>% 
  select (-year_birth, -Name_mother, -Name, -Date_of_birth, -Name_father, - Date_of_death)
total_foals <- left_join (total_foals, foals_nextyear, by = c("name", "year_observation"))

write_xlsx (total_foals,"przewalski/data/total_foals.xlsx") # saving this dataset to check manually

# 6,7 - with foal and pregnant (explanatory variables)
# with foal: if female had a foal born
# from the harem stallion in year of or year before observation
total_foals <- total_foals %>% 
  mutate(with_foal = case_when(
    (!is.na(name_foal) & # foal born in year of obs
       dob_foal <= date & # foal born before observation
       father_of_foal == previous_harem) | 
      (!is.na(foal_prev_year) & # foal born in year before obs
         father_of_foal_prev_year == previous_harem) ~ 1,
    TRUE ~ 0))

# pregnant: if female was pregnant from focal harem stallion at the time of observation
total_foals <- total_foals %>% 
  mutate(pregnant = case_when(
    (!is.na(name_foal) &  # foal born in year of obs
       dob_foal > date &  # foal born after observation
       father_of_foal == previous_harem)  |
      (!is.na(foal_next_year) & 
         dob_foal_next_year - date < 330 & # gestation period of 11 months
         father_of_foal_next_year == previous_harem) ~ 1,  
    TRUE ~ 0))

# foal that survived to the date of observation
total_foals  <- total_foals %>% 
  mutate(foal_alive =  case_when( !is.na(name_foal) & # foal born in year of obs
                                    dob_foal <= date & # foal born before observation
                                    father_of_foal == previous_harem 
                                  &
                                    (is.na(death_foal) | (!is.na(death_foal)& death_foal > date))
                                  | 
                                    (!is.na(foal_prev_year) & # foal born in year before obs
                                       father_of_foal_prev_year == previous_harem) &
                                    (is.na(death_foal_prev_year) | (!is.na(death_foal_prev_year) & death_foal_prev_year > date))
                                  ~ 1,
                                  TRUE ~ 0 ))

# 10 - harem size
# new dataset with adult harem size of each harem at each date
haremsize <- total_foals %>% 
  filter   (!age < 730, # adult horses
            ! (harem == Name_father & age < (1095)), # post dispersal females only
            Gender == 2) %>% # bachelors who may join occasionally not included
  group_by(date, harem) %>% 
  summarise(harem_size = length(name))
haremsize$date <- as.Date(haremsize$date)

total_foals_harem <- left_join(total_foals, haremsize, by= c("harem", "date"))

total_foals_harem <- total_foals_harem %>% 
  arrange(date) %>% 
  group_by(name) %>% 
  mutate( previous_haremsize =  lag(harem_size)) %>% 
  # this is the variable of interest (i.e. focal harem)
  ungroup()

total_foals <- total_foals_harem

# age class


total_foals <- total_foals %>% 
  mutate(age_class = 
           case_when (age <= 1460 ~ "a" , # younger than 4
                      age  > 1460 & age <= 3650 ~ "b", # between 4 and 10
                      age > 3650 ~ "c") ) # older than 10
str(total_foals$age_class)



# preparing data for modelling----
for_model <-  total_foals %>% 
  filter (Gender == 2 & age > 730) %>% 
  # only females older than 2
  filter(! (previous_harem == Name_father& age < 1095)) %>% 
  # post-dispersal females only (should disperse by 3) 
  filter (date > as.Date("2008-01-01")) %>%
  # observations from 2008 onwards
  filter (!is.na(change),
          !is.na(age),
          !is.na(with_foal),
          !is.na(pregnant),
          !is.na(harem_size))
# only keep data where there is data for all variables for models to be comparable
str(for_model)
for_model$age_class <- as.factor(for_model$age_class)
for_model$year_observation <- as.factor(for_model$year_observation)

# scale time last observed for convergence
for_model$time_last_observed_scaled <- scale(for_model$time_to_last_obs)
for_model$time_last_observed_scaled <- as.numeric(for_model$time_last_observed_scaled) # variable was acting weird
for_model$prev_haremsize_scaled <- scale(for_model$previous_haremsize)
for_model$prev_haremsize_scaled <- as.numeric (for_model$prev_haremsize_scaled)

# define optimizer
control <- glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000))

# visualising raw data----
(plot <- ggplot(for_model, aes(x= with_foal, y = change))+geom_point()+geom_jitter())
(plot2 <- ggplot(for_model, aes(x= age_class, y = change))+geom_point()+geom_jitter())
(plot3 <- ggplot(for_model, aes(x= pregnant, y = change))+geom_point()+geom_jitter())
(plot4  <- ggplot(for_model, aes (x = foal_alive, y = change))+ geom_point()+geom_jitter())

# models ----
# null model 
null_model <- glmer (change ~ 1+ (1|year_observation)  +(1|previous_harem/name), data = for_model, family = binomial)
AIC(null_model)

# no interactions
simple_model <- glmer(change ~ time_last_observed_scaled + age_class + with_foal + pregnant+  prev_haremsize_scaled+
                            (1|year_observation)  + (1|previous_harem/name) ,  
                          data = for_model, family = binomial,
                          control = control)

summary(simple_model)
# AIC 7770.8   
r2_simple <- r.squaredGLMM(simple_model)
r2_simple

# all interactions
all_interactions_model <- glmer (change ~ time_last_observed_scaled +
                                       age_class*pregnant + age_class*with_foal + age_class*prev_haremsize_scaled+
                                       (1|year_observation)  +(1|previous_harem/name) ,  
                                     data = for_model, family = binomial,
                                     control = control)
summary(all_interactions_model) # aic 
r2_interactions_all <- r.squaredGLMM(all_interactions_model)
r2_interactions_all # 0.088

interactions_foal_pregnant <- glmer (change ~ time_last_observed_scaled +
                                      age_class*pregnant + age_class*with_foal+ 
                                       previous_haremsize+
                                      (1|year_observation)  +(1|previous_harem/name) ,  
                                    data = for_model, family = binomial,
                                    control = control)
summary (interactions_foal_pregnant)

interaction_pregnant <- glmer (change ~ time_last_observed_scaled +
                                     age_class*pregnant + with_foal + prev_haremsize_scaled+
                                     (1|year_observation)  +(1|previous_harem/name) ,  
                                   data = for_model, family = binomial,
                                   control = control)
summary(interaction_pregnant)

model2_interactions_foal_pregnant<- glmer(change ~ time_last_observed_scaled + age_class*pregnant +
                                age_class*foal_alive + prev_haremsize_scaled+ 
                                (1|year_observation)  +(1|previous_harem/name) ,  
                              data = for_model, family = binomial,
                              control = control)

summary(model2_interactions_foal_pregnant) # aic 7749
r_squared_surviving <- r.squaredGLMM(model2_interactions_foal_pregnant) 
r_squared_surviving 

# no interactions
model2_simple <- glmer(change ~ time_last_observed_scaled + age_class + foal_alive + pregnant+  prev_haremsize_scaled+
                        (1|year_observation)  + (1|previous_harem/name) ,  
                      data = for_model, family = binomial,
                      control = control)

summary(model2_simple)
# AIC  7764
r2_simple <- r.squaredGLMM(model2_simple)
r2_simple


# all interactions
all_interactions_model2 <- glmer (change ~ time_last_observed_scaled +
                                   age_class*pregnant + age_class*foal_alive + age_class*prev_haremsize_scaled+
                                   (1|year_observation)  +(1|previous_harem/name) ,  
                                 data = for_model, family = binomial,
                                 control = control)
summary(all_interactions_model2) # aic 
r2_interactions_all <- r.squaredGLMM(all_interactions_model2)
r2_interactions_all # 0.088



model2_interaction_pregnant <- glmer (change ~ time_last_observed_scaled +
                                 age_class*pregnant + foal_alive + prev_haremsize_scaled+
                                 (1|year_observation)  +(1|previous_harem/name) ,  
                               data = for_model, family = binomial,
                               control = control)
summary (model2_interaction_pregnant)

# calculating probabilities from model output----
 # intercept: probability (p) of change = 1 when foal = 0 and pregnant = 0, everything else at reference level
# i.e. p of change = 1 when foal = 0 and pregnant = 0 for age class a. 
plogis(-1.82976) 
# 0.1382669
# p of change = 1 for age class b when foal = 0 and pregnant = 0, everything else at reference level
 plogis(-1.82976-0.39465 )
# 0.09757978
 # p of change = 1 for age class c when foal = 0 and pregnant = 0, everything else at reference level
 plogis(-1.82976-0.38680)
# 0.09827322
 # effect of pregnant on age class a: p of change = 1 when pregnant = 1, for age class a
 plogis(-1.82976-2.14431)
# 0.01844997
 # effect of pregnant on age class b: p of change = 1 when pregnant = 1, for age class b
 plogis(-1.82976-0.39465-2.14431+1.05973)
# 0.03526406
 # effect of pregnant on age class c: p of change = 1 when pregnant = 1, for age class c
 plogis(-1.82976 -0.38680 -2.14431+0.90357)
# 0.0305519
 # effect of foal on age class a: p of change = 1 when foal = 1, for age class a
 plogis(-1.82976-1.50365)
# 0.03444265
 # effect of foal on age class b: p of change = 1 when foal = 1, for age class b
 plogis(-1.82976-1.50365-0.39465+0.99484 )
# 0.06104135
 # effect of foal on age class b: p of change = 1 when foal = 1, for age class c
 plogis(-1.82976-1.50365-0.38680+ 1.10578 )
# 0.06821548

# attempt 1 to get CI----
 # Standard errors for the coefficients
 se_intercept <- 0.23775 # Standard error for intercept coefficient
 se_pregnant <- 0.23879   # Standard error for pregnant coefficient
 se_foal <- 0.31173   # Standard error for foal coefficient
 se_preg_b <- 0.27479    # Standard error for pregnant coefficient for age class b
 se_preg_c <- 0.35580    # Standard error for pregnant coefficient for age class c
 se_foal_b <- 0.33162    # Standard error for foal coefficient for age class b
 se_foal_c <- 0.37296    # Standard error for foal coefficient for age class c
 
 # Z-score for desired confidence level (e.g., 95% confidence interval)
 z <- qnorm(0.975) # for 95% CI
 
 # Calculate confidence intervals
 ci_preg_0_a <- c(plogis(-1.82976 - z * se_intercept), plogis(-1.82976 + z * se_intercept))
 ci_preg_1_a <- c(plogis(-1.82976 - 2.14431 - z * (se_intercept + se_pregnant)), plogis(-1.82976 - 2.14431 + z * (se_intercept + se_pregnant)))
 
 ci_preg_0_b <- c(plogis(-1.82976 - 0.39465 - z * (se_intercept + se_pregnant + se_preg_b)), plogis(-1.82976 - 0.39465 + z * (se_intercept + se_pregnant + se_preg_b)))
 ci_preg_1_b <- c(plogis(-1.82976 - 0.39465 - 2.14431 + 1.05973 - z * (se_intercept + se_pregnant + se_preg_b)), plogis(-1.82976 - 0.39465 - 2.14431 + 1.05973 + z * (se_intercept + se_pregnant + se_preg_b)))
 
 ci_preg_0_c <- c(plogis(-1.82976 - 0.38680 - z * (se_intercept + se_pregnant + se_preg_c)), plogis(-1.82976 - 0.38680 + z * (se_intercept + se_pregnant + se_preg_c)))
 ci_preg_1_c <- c(plogis(-1.82976 - 0.38680 - 2.14431 + 0.90357 - z * (se_intercept + se_pregnant + se_preg_c)), plogis(-1.82976 - 0.38680 - 2.14431 + 0.90357 + z * (se_intercept + se_pregnant + se_preg_c)))
 
 # Repeat the process for foal
 # Calculate confidence intervals
 ci_foal_0_a <- c(plogis(-1.82976 - z * se_intercept), plogis(-1.82976 + z * se_intercept))
 ci_foal_1_a <- c(plogis(-1.82976 - 1.50365 - z * (se_intercept + se_foal)), plogis(-1.82976 - 1.50365 + z * (se_intercept + se_foal)))
 
 ci_foal_0_b <- c(plogis(-1.82976 - 0.39465 - z * (se_intercept + se_foal + se_foal_b)), plogis(-1.82976 - 0.39465 + z * (se_intercept + se_foal + se_foal_b)))
 ci_foal_1_b <- c(plogis(-1.82976 - 0.39465 - 1.50365 + 0.99484 - z * (se_intercept + se_foal + se_foal_b)), plogis(-1.82976 - 0.39465 - 1.50365 + 0.99484 + z * (se_intercept + se_foal + se_foal_b)))
 
 ci_foal_0_c <- c(plogis(-1.82976 - 0.38680 - z * (se_intercept + se_foal + se_foal_c)), plogis(-1.82976 - 0.38680 + z * (se_intercept + se_foal + se_foal_c)))
 ci_foal_1_c <- c(plogis(-1.82976 - 0.38680 - 1.50365 + 1.10578 - z * (se_intercept + se_foal + se_foal_c)), plogis(-1.82976 - 0.38680 - 1.50365 + 1.10578 + z * (se_intercept + se_foal + se_foal_c)))
 
 # Display results
 cat("Pregnant = 0, Age Class A CI:", ci_preg_0_a, "\n")
 cat("Pregnant = 1, Age Class A CI:", ci_preg_1_a, "\n")
 cat("Pregnant = 0, Age Class B CI:", ci_preg_0_b, "\n")
 cat("Pregnant = 1, Age Class B CI:", ci_preg_1_b, "\n")
 cat("Pregnant = 0, Age Class C CI:", ci_preg_0_c, "\n")
 cat("Pregnant = 1, Age Class C CI:", ci_preg_1_c, "\n")
 
 cat("Foal = 0, Age Class A CI:", ci_foal_0_a, "\n")
 cat("Foal = 1, Age Class A CI:", ci_foal_1_a, "\n")
 cat("Foal = 0, Age Class B CI:", ci_foal_0_b, "\n")
 cat("Foal = 1, Age Class B CI:", ci_foal_1_b, "\n")
 cat("Foal = 0, Age Class C CI:", ci_foal_0_c, "\n")
 cat("Foal = 1, Age Class C CI:", ci_foal_1_c, "\n")
 
 
# attempt 2 ----
 # Given probabilities and standard errors
 prob_preg_0_a <- plogis(-1.82976)
 se_preg_0_a <- 0.23775 * prob_preg_0_a * (1 - prob_preg_0_a)
 prob_preg_1_a <- plogis(-1.82976 - 2.14431)
 se_preg_1_a <- 0.23879 * prob_preg_1_a * (1 - prob_preg_1_a)
 
 prob_preg_0_b <- plogis(-1.82976 - 0.39465)
 se_preg_b <- 0.27479 * prob_preg_0_b * (1 - prob_preg_0_b)
 prob_preg_1_b <- plogis(-1.82976 - 0.39465 - 2.14431 + 1.05973)
 se_preg_1_b <- 0.27479 * prob_preg_1_b * (1 - prob_preg_1_b)
 
 prob_preg_0_c <- plogis(-1.82976 - 0.38680)
 se_preg_c <- 0.35580 * prob_preg_0_c * (1 - prob_preg_0_c)
 prob_preg_1_c <- plogis(-1.82976 - 0.38680 - 2.14431 + 0.90357)
 se_preg_1_c <- 0.35580 * prob_preg_1_c * (1 - prob_preg_1_c)
 
 prob_foal_0_a <- plogis(-1.82976 - 1.50365)
 se_foal_a <- 0.31173 * prob_foal_0_a * (1 - prob_foal_0_a)
 prob_foal_1_a <- plogis(-1.82976 - 1.50365 - 2.14431 + 0.99484)
 se_foal_1_a <- 0.31173 * prob_foal_1_a * (1 - prob_foal_1_a)
 
 prob_foal_0_b <- plogis(-1.82976 - 1.50365 - 0.39465)
 se_foal_b <- 0.33162 * prob_foal_0_b * (1 - prob_foal_0_b)
 prob_foal_1_b <- plogis(-1.82976 - 1.50365 - 0.39465 - 2.14431 + 1.10578)
 se_foal_1_b <- 0.33162 * prob_foal_1_b * (1 - prob_foal_1_b)
 
 prob_foal_0_c <- plogis(-1.82976 - 1.50365 - 0.38680)
 se_foal_c <- 0.37296 * prob_foal_0_c * (1 - prob_foal_0_c)
 prob_foal_1_c <- plogis(-1.82976 - 1.50365 - 0.38680 - 2.14431 + 1.10578)
 se_foal_1_c <- 0.37296 * prob_foal_1_c * (1 - prob_foal_1_c)
 
 # Z-score for desired confidence level (e.g., 95% confidence interval)
 z <- qnorm(0.975) # for 95% CI
 
 # Calculate confidence intervals for pregnant probabilities
 ci_preg_0_a <- c(prob_preg_0_a - z * se_preg_0_a, prob_preg_0_a + z * se_preg_0_a)
 ci_preg_1_a <- c(prob_preg_1_a - z * se_preg_1_a, prob_preg_1_a + z * se_preg_1_a)
 ci_preg_0_b <- c(prob_preg_0_b - z * se_preg_b, prob_preg_0_b + z * se_preg_b)
 ci_preg_1_b <- c(prob_preg_1_b - z * se_preg_1_b, prob_preg_1_b + z * se_preg_1_b)
 ci_preg_0_c <- c(prob_preg_0_c - z * se_preg_c, prob_preg_0_c + z * se_preg_c)
 ci_preg_1_c <- c(prob_preg_1_c - z * se_preg_1_c, prob_preg_1_c + z * se_preg_1_c)
 
 # Calculate confidence intervals for foal probabilities
 ci_foal_0_a <- c(prob_foal_0_a - z * se_foal_a, prob_foal_0_a + z * se_foal_a)
 ci_foal_1_a <- c(prob_foal_1_a - z * se_foal_1_a, prob_foal_1_a + z * se_foal_1_a)
 ci_foal_0_b <- c(prob_foal_0_b - z * se_foal_b, prob_foal_0_b + z * se_foal_b)
 ci_foal_1_b <- c(prob_foal_1_b - z * se_foal_1_b, prob_foal_1_b + z * se_foal_1_b)
 ci_foal_0_c <- c(prob_foal_0_c - z * se_foal_c, prob_foal_0_c + z * se_foal_c)
 ci_foal_1_c <- c(prob_foal_1_c - z * se_foal_1_c, prob_foal_1_c + z * se_foal_1_c)
 
 # Display results
 cat("Pregnant = 0, Age Class A CI:", ci_preg_0_a, "\n")
 cat("Pregnant = 1, Age Class A CI:", ci_preg_1_a, "\n")
 cat("Pregnant = 0, Age Class B CI:", ci_preg_0_b, "\n")
 cat("Pregnant = 1, Age Class B CI:", ci_preg_1_b, "\n")
 cat("Pregnant = 0, Age Class C CI:", ci_preg_0_c, "\n")
 cat("Pregnant = 1, Age Class C CI:", ci_preg_1_c, "\n")
 
 cat("Foal = 0, Age Class A CI:", ci_foal_0_a, "\n")
 cat("Foal = 1, Age Class A CI:", ci_foal_1_a, "\n")
 cat("Foal = 0, Age Class B CI:", ci_foal_0_b, "\n")
 cat("Foal = 1, Age Class B CI:", ci_foal_1_b, "\n")
 cat("Foal = 0, Age Class C CI:", ci_foal_0_c, "\n")
 cat("Foal = 1, Age Class C CI:", ci_foal_1_c, "\n")
 
 # attempt 3 not bad but slightly wrong----
 # Given probabilities and standard errors
 prob_preg_0_a <- plogis(-1.82976)
 se_preg_0_a <- 0.23775 * prob_preg_0_a * (1 - prob_preg_0_a)
 prob_preg_1_a <- plogis(-1.82976 - 2.14431)
 se_preg_1_a <- 0.23879 * prob_preg_1_a * (1 - prob_preg_1_a)
 

 prob_preg_1_b <- plogis(-1.82976 - 0.39465 - 2.14431 + 1.05973)
 se_preg_1_b <- 0.27479 * prob_preg_1_b * (1 - prob_preg_1_b)
 
 prob_preg_0_c <- plogis(-1.82976 - 0.38680)
 se_preg_c <- 0.35580 * prob_preg_0_c * (1 - prob_preg_0_c)
 prob_preg_1_c <- plogis(-1.82976 - 0.38680 - 2.14431 + 0.90357)
 se_preg_1_c <- 0.35580 * prob_preg_1_c * (1 - prob_preg_1_c)
 
 prob_foal_0_a <- plogis(-1.82976 - 1.50365)
 se_foal_a <- 0.31173 * prob_foal_0_a * (1 - prob_foal_0_a)
 prob_foal_1_a <- plogis(-1.82976 - 1.50365 - 0.39465 + 0.99484)
 se_foal_1_a <- 0.31173 * prob_foal_1_a * (1 - prob_foal_1_a)
 
 prob_foal_0_b <- plogis(-1.82976 - 1.50365 - 0.39465)
 se_foal_b <- 0.33162 * prob_foal_0_b * (1 - prob_foal_0_b)
 prob_foal_1_b <- plogis(-1.82976 - 1.50365 - 0.39465 + 0.99484)
 se_foal_1_b <- 0.33162 * prob_foal_1_b * (1 - prob_foal_1_b)
 
 prob_foal_0_c <- plogis(-1.82976 - 1.50365 - 0.38680)
 se_foal_c <- 0.37296 * prob_foal_0_c * (1 - prob_foal_0_c)
 prob_foal_1_c <- plogis(-1.82976 - 1.50365 - 0.38680 + 1.10578)
 se_foal_1_c <- 0.37296 * prob_foal_1_c * (1 - prob_foal_1_c)
 
 # Z-score for desired confidence level (e.g., 95% confidence interval)
 z <- qnorm(0.975) # for 95% CI
 
 # Calculate confidence intervals for pregnant probabilities
 ci_preg_0_a <- c(prob_preg_0_a - z * se_preg_0_a, prob_preg_0_a + z * se_preg_0_a)
 ci_preg_1_a <- c(prob_preg_1_a - z * se_preg_1_a, prob_preg_1_a + z * se_preg_1_a)
 ci_preg_0_b <- c(prob_preg_0_b - z * se_preg_b, prob_preg_0_b + z * se_preg_b)
 ci_preg_1_b <- c(prob_preg_1_b - z * se_preg_1_b, prob_preg_1_b + z * se_preg_1_b)
 ci_preg_0_c <- c(prob_preg_0_c - z * se_preg_c, prob_preg_0_c + z * se_preg_c)
 ci_preg_1_c <- c(prob_preg_1_c - z * se_preg_1_c, prob_preg_1_c + z * se_preg_1_c)
 
 # Calculate confidence intervals for foal probabilities
 ci_foal_0_a <- c(prob_foal_0_a - z * se_foal_a, prob_foal_0_a + z * se_foal_a)
 ci_foal_1_a <- c(prob_foal_1_a - z * se_foal_1_a, prob_foal_1_a + z * se_foal_1_a)
 ci_foal_0_b <- c(prob_foal_0_b - z * se_foal_b, prob_foal_0_b + z * se_foal_b)
 ci_foal_1_b <- c(prob_foal_1_b - z * se_foal_1_b, prob_foal_1_b + z * se_foal_1_b)
 ci_foal_0_c <- c(prob_foal_0_c - z * se_foal_c, prob_foal_0_c + z * se_foal_c)
 ci_foal_1_c <- c(prob_foal_1_c - z * se_foal_1_c, prob_foal_1_c + z * se_foal_1_c)
 
 # Display results
 cat("Pregnant = 0, Age Class A CI:", ci_preg_0_a, "\n")
 cat("Pregnant = 1, Age Class A CI:", ci_preg_1_a, "\n")
 cat("Pregnant = 0, Age Class B CI:", ci_preg_0_b, "\n")
 cat("Pregnant = 1, Age Class B CI:", ci_preg_1_b, "\n")
 cat("Pregnant = 0, Age Class C CI:", ci_preg_0_c, "\n")
 cat("Pregnant = 1, Age Class C CI:", ci_preg_1_c, "\n")
 
 cat("Foal = 0, Age Class A CI:", ci_foal_0_a, "\n")
 cat("Foal = 1, Age Class A CI:", ci_foal_1_a, "\n")
 cat("Foal = 0, Age Class B CI:", ci_foal_0_b, "\n")
 cat("Foal = 1, Age Class B CI:", ci_foal_1_b, "\n")
 cat("Foal = 0, Age Class C CI:", ci_foal_0_c, "\n")
 cat("Foal = 1, Age Class C CI:", ci_foal_1_c, "\n")
 
 # attempt 4----
 # Given probabilities and standard errors
 prob_preg_0_a <- plogis(-1.82976)
 se_preg_0_a <- 0.23775 * prob_preg_0_a * (1 - prob_preg_0_a)
 
 prob_preg_1_a <- plogis(-1.82976 - 2.14431)
 se_preg_1_a <- 0.23879 * prob_preg_1_a * (1 - prob_preg_1_a)
 
 prob_preg_1_b <- plogis(-1.82976 - 0.39465 - 2.14431 + 1.05973)
 se_preg_1_b <- 0.27479 * prob_preg_1_b * (1 - prob_preg_1_b)
 
 prob_preg_0_c <- plogis(-1.82976 - 0.38680)
 se_preg_c <- 0.35580 * prob_preg_0_c * (1 - prob_preg_0_c)
 
 prob_preg_1_c <- plogis(-1.82976 - 0.38680 - 2.14431 + 0.90357)
 se_preg_1_c <- 0.35580 * prob_preg_1_c * (1 - prob_preg_1_c)
 
 prob_foal_0_a <- plogis(-1.82976 )
 se_foal_a <- 0.31173 * prob_foal_0_a * (1 - prob_foal_0_a)
 
 prob_foal_1_a <- plogis(-1.82976 - 1.50365 - 0.39465 + 0.99484)
 se_foal_1_a <- 0.31173 * prob_foal_1_a * (1 - prob_foal_1_a)
 
 prob_foal_0_b <- plogis(-1.82976 - 1.50365 - 0.39465)
 se_foal_b <- 0.33162 * prob_foal_0_b * (1 - prob_foal_0_b)
 
 prob_foal_1_b <- plogis(-1.82976 - 1.50365 - 0.39465 + 0.99484)
 se_foal_1_b <- 0.33162 * prob_foal_1_b * (1 - prob_foal_1_b)
 
 prob_foal_0_c <- plogis(-1.82976 - 1.50365 - 0.38680)
 se_foal_c <- 0.37296 * prob_foal_0_c * (1 - prob_foal_0_c)
 
 prob_foal_1_c <- plogis(-1.82976 - 1.50365 - 0.38680 + 1.10578)
 se_foal_1_c <- 0.37296 * prob_foal_1_c * (1 - prob_foal_1_c)
 
 # Z-score for desired confidence level (e.g., 95% confidence interval)
 z <- qnorm(0.975) # for 95% CI
 
 # Calculate confidence intervals for pregnant probabilities
 ci_preg_0_a <- c(prob_preg_0_a - z * se_preg_0_a, prob_preg_0_a + z * se_preg_0_a)
 ci_preg_1_a <- c(prob_preg_1_a - z * se_preg_1_a, prob_preg_1_a + z * se_preg_1_a)
 ci_preg_1_b <- c(prob_preg_1_b - z * se_preg_1_b, prob_preg_1_b + z * se_preg_1_b)
 ci_preg_0_c <- c(prob_preg_0_c - z * se_preg_c, prob_preg_0_c + z * se_preg_c)
 ci_preg_1_c <- c(prob_preg_1_c - z * se_preg_1_c, prob_preg_1_c + z * se_preg_1_c)
 
 # Calculate confidence intervals for foal probabilities
 ci_foal_0_a <- c(prob_foal_0_a - z * se_foal_a, prob_foal_0_a + z * se_foal_a)
 ci_foal_1_a <- c(prob_foal_1_a - z * se_foal_1_a, prob_foal_1_a + z * se_foal_1_a)
 ci_foal_0_b <- c(prob_foal_0_b - z * se_foal_b, prob_foal_0_b + z * se_foal_b)
 ci_foal_1_b <- c(prob_foal_1_b - z * se_foal_1_b, prob_foal_1_b + z * se_foal_1_b)
 ci_foal_0_c <- c(prob_foal_0_c - z * se_foal_c, prob_foal_0_c + z * se_foal_c)
 ci_foal_1_c <- c(prob_foal_1_c - z * se_foal_1_c, prob_foal_1_c + z * se_foal_1_c)
 
 # Display results
 cat("Pregnant = 0, Age Class A CI:", ci_preg_0_a, "\n")
 cat("Pregnant = 1, Age Class A CI:", ci_preg_1_a, "\n")
 cat("Pregnant = 1, Age Class B CI:", ci_preg_1_b, "\n")
 cat("Pregnant = 0, Age Class C CI:", ci_preg_0_c, "\n")
 cat("Pregnant = 1, Age Class C CI:", ci_preg_1_c, "\n")
 
 cat("Foal = 0, Age Class A CI:", ci_foal_0_a, "\n")
 cat("Foal = 1, Age Class A CI:", ci_foal_1_a, "\n")
 cat("Foal = 0, Age Class B CI:", ci_foal_0_b, "\n")
 cat("Foal = 1, Age Class B CI:", ci_foal_1_b, "\n")
 cat("Foal = 0, Age Class C CI:", ci_foal_0_c, "\n")
 cat("Foal = 1, Age Class C CI:", ci_foal_1_c, "\n")
 
 
 # attempt 5 -----
 # Given probabilities and standard errors
 prob_preg_0_a <- plogis(-1.82976)
 se_preg_0_a <- 0.23775 * prob_preg_0_a * (1 - prob_preg_0_a)
 
 prob_preg_1_a <- plogis(-1.82976 - 2.14431)
 se_preg_1_a <- 0.23879 * prob_preg_1_a * (1 - prob_preg_1_a)
 
 prob_preg_0_b <- plogis(-1.82976 - 0.39465)
 se_preg_b <- 0.23775   * prob_preg_0_b * (1 - prob_preg_0_b)
 
 prob_preg_1_b <- plogis(-1.82976 - 0.39465 - 2.14431 + 1.05973)
 se_preg_1_b <- 0.27479    * prob_preg_1_b * (1 - prob_preg_1_b)
 
 prob_preg_0_c <- plogis(-1.82976 - 0.38680)
 se_preg_c <- 0.23775   * prob_preg_0_c * (1 - prob_preg_0_c)
 
 prob_preg_1_c <- plogis(-1.82976 - 0.38680 - 2.14431 + 0.90357)
 se_preg_1_c <- 0.35580 * prob_preg_1_c * (1 - prob_preg_1_c)
 
 prob_foal_0_a <- plogis(-1.82976 )
 se_foal_a <- 0.23775   * prob_foal_0_a * (1 - prob_foal_0_a)
 
 prob_foal_1_a <- plogis(-1.82976 - 1.50365 )
 se_foal_1_a <- 0.31173 * prob_foal_1_a * (1 - prob_foal_1_a)
 
 prob_foal_0_b <- plogis(-1.82976  - 0.39465)
 se_foal_b <- 0.23775 * prob_foal_0_b * (1 - prob_foal_0_b)
 
 prob_foal_1_b <- plogis(-1.82976 - 1.50365 - 0.39465 + 0.99484)
 se_foal_1_b <- 0.33162    * prob_foal_1_b * (1 - prob_foal_1_b)
 
 prob_foal_0_c <- plogis(-1.82976  - 0.38680)
 se_foal_c <- 0.23775   * prob_foal_0_c * (1 - prob_foal_0_c)
 
 prob_foal_1_c <- plogis(-1.82976 - 1.50365 - 0.38680 + 1.10578)
 se_foal_1_c <- 0.37296 * prob_foal_1_c * (1 - prob_foal_1_c)
 
 # Z-score for desired confidence level (e.g., 95% confidence interval)
 z <- qnorm(0.975) # for 95% CI
 
 # Calculate confidence intervals for pregnant probabilities
 ci_preg_0_a <- c(prob_preg_0_a - z * se_preg_0_a, prob_preg_0_a + z * se_preg_0_a)
 ci_preg_1_a <- c(prob_preg_1_a - z * se_preg_1_a, prob_preg_1_a + z * se_preg_1_a)
 ci_preg_0_b <- c(prob_preg_0_b - z * se_preg_b, prob_preg_0_b + z * se_preg_b)
 ci_preg_1_b <- c(prob_preg_1_b - z * se_preg_1_b, prob_preg_1_b + z * se_preg_1_b)
 ci_preg_0_c <- c(prob_preg_0_c - z * se_preg_c, prob_preg_0_c + z * se_preg_c)
 ci_preg_1_c <- c(prob_preg_1_c - z * se_preg_1_c, prob_preg_1_c + z * se_preg_1_c)
 
 # Calculate confidence intervals for foal probabilities
 ci_foal_0_a <- c(prob_foal_0_a - z * se_foal_a, prob_foal_0_a + z * se_foal_a)
 ci_foal_1_a <- c(prob_foal_1_a - z * se_foal_1_a, prob_foal_1_a + z * se_foal_1_a)
 ci_foal_0_b <- c(prob_foal_0_b - z * se_foal_b, prob_foal_0_b + z * se_foal_b)
 ci_foal_1_b <- c(prob_foal_1_b - z * se_foal_1_b, prob_foal_1_b + z * se_foal_1_b)
 ci_foal_0_c <- c(prob_foal_0_c - z * se_foal_c, prob_foal_0_c + z * se_foal_c)
 ci_foal_1_c <- c(prob_foal_1_c - z * se_foal_1_c, prob_foal_1_c + z * se_foal_1_c)
 
 # Display results
 cat("Pregnant = 0, Age Class A CI:", ci_preg_0_a, "\n")
 cat("Pregnant = 1, Age Class A CI:", ci_preg_1_a, "\n")
 cat("Pregnant = 0, Age Class B CI:", ci_preg_0_b, "\n")
 cat("Pregnant = 1, Age Class B CI:", ci_preg_1_b, "\n")
 cat("Pregnant = 0, Age Class C CI:", ci_preg_0_c, "\n")
 cat("Pregnant = 1, Age Class C CI:", ci_preg_1_c, "\n")
 
 cat("Foal = 0, Age Class A CI:", ci_foal_0_a, "\n")
 cat("Foal = 1, Age Class A CI:", ci_foal_1_a, "\n")
 cat("Foal = 0, Age Class B CI:", ci_foal_0_b, "\n")
 cat("Foal = 1, Age Class B CI:", ci_foal_1_b, "\n")
 cat("Foal = 0, Age Class C CI:", ci_foal_0_c, "\n")
 cat("Foal = 1, Age Class C CI:", ci_foal_1_c, "\n")
 
 # attempt 6 ----
 # Given intercepts and differences
 intercept <- -1.82976
 intercept_diff_b <- -0.39465
 intercept_diff_c <- -0.38680
 pregnant_effect_a <- -2.14431
 pregnant_effect_b <- -2.14431 + 1.05973
 pregnant_effect_c <- -2.14431 + 0.90357
 foal_effect_a <- -1.50365
 foal_effect_b <- -1.50365 + 0.99484
 foal_effect_c <- -1.50365 + 1.10578
 
 # Calculate probabilities
 prob_preg_0_a <- plogis(intercept)
 prob_preg_0_b <- plogis(intercept + intercept_diff_b)
 prob_preg_0_c <- plogis(intercept + intercept_diff_c)
 prob_preg_1_a <- plogis(intercept + pregnant_effect_a)
 prob_preg_1_b <- plogis(intercept + intercept_diff_b  + pregnant_effect_b)
 prob_preg_1_c <- plogis(intercept + intercept_diff_c + pregnant_effect_c)
 
 prob_foal_0_a <- plogis(intercept)
 prob_foal_0_b <- plogis(intercept + intercept_diff_b)
 prob_foal_0_c <- plogis(intercept + intercept_diff_c)
 prob_foal_1_a <- plogis(intercept + foal_effect_a)
 prob_foal_1_b <- plogis(intercept + intercept_diff_b  +foal_effect_b)
 prob_foal_1_c <- plogis(intercept + intercept_diff_c +foal_effect_c)
 
 # Calculate standard errors
 se_preg_0_a <- 0.23775 * prob_preg_0_a * (1 - prob_preg_0_a)
 se_preg_0_b <- 0.10833 * prob_preg_0_b * (1 - prob_preg_0_b)
 se_preg_0_c <- 0.13911 * prob_preg_0_c * (1 - prob_preg_0_c)
 se_preg_1_a <- 0.23879 * prob_preg_1_a * (1 - prob_preg_1_a)
 se_preg_1_b <- 0.27479 * prob_preg_1_b * (1 - prob_preg_1_b)
 se_preg_1_c <- 0.35580 * prob_preg_1_c * (1 - prob_preg_1_c)
 
 se_foal_0_a <- 0.31173 * prob_foal_0_a * (1 - prob_foal_0_a)
 se_foal_0_b <- 0.33162 * prob_foal_0_b * (1 - prob_foal_0_b)
 se_foal_0_c <- 0.37296 * prob_foal_0_c * (1 - prob_foal_0_c)
 se_foal_1_a <- 0.31173 * prob_foal_1_a * (1 - prob_foal_1_a)
 se_foal_1_b <- 0.33162 * prob_foal_1_b * (1 - prob_foal_1_b)
 se_foal_1_c <- 0.37296 * prob_foal_1_c * (1 - prob_foal_1_c)
 
 # Z-score for desired confidence level (e.g., 95% confidence interval)
 z <- qnorm(0.975) # for 95% CI
 
 # Calculate confidence intervals for pregnant probabilities
 ci_preg_0_a <- c(prob_preg_0_a - z * se_preg_0_a, prob_preg_0_a + z * se_preg_0_a)
 ci_preg_0_b <- c(prob_preg_0_b - z * se_preg_0_b, prob_preg_0_b + z * se_preg_0_b)
 ci_preg_0_c <- c(prob_preg_0_c - z * se_preg_0_c, prob_preg_0_c + z * se_preg_0_c)
 ci_preg_1_a <- c(prob_preg_1_a - z * se_preg_1_a, prob_preg_1_a + z * se_preg_1_a)
 ci_preg_1_b <- c(prob_preg_1_b - z * se_preg_1_b, prob_preg_1_b + z * se_preg_1_b)
 ci_preg_1_c <- c(prob_preg_1_c - z * se_preg_1_c, prob_preg_1_c + z * se_preg_1_c)
 
 # Calculate confidence intervals for foal probabilities
 ci_foal_0_a <- c(prob_foal_0_a - z * se_foal_0_a, prob_foal_0_a + z * se_foal_0_a)
 ci_foal_0_b <- c(prob_foal_0_b - z * se_foal_0_b, prob_foal_0_b + z * se_foal_0_b)
 ci_foal_0_c <- c(prob_foal_0_c - z * se_foal_0_c, prob_foal_0_c + z * se_foal_0_c)
 ci_foal_1_a <- c(prob_foal_1_a - z * se_foal_1_a, prob_foal_1_a + z * se_foal_1_a)
 ci_foal_1_b <- c(prob_foal_1_b - z * se_foal_1_b, prob_foal_1_b + z * se_foal_1_b)
 ci_foal_1_c <- c(prob_foal_1_c - z * se_foal_1_c, prob_foal_1_c + z * se_foal_1_c)
 
 # Display results
 cat("Pregnant = 0, Age Class A CI:", ci_preg_0_a, "\n")
 cat("Pregnant = 1, Age Class A CI:", ci_preg_1_a, "\n")
 cat("Pregnant = 0, Age Class B CI:", ci_preg_0_b, "\n")
 cat("Pregnant = 1, Age Class B CI:", ci_preg_1_b, "\n")
 cat("Pregnant = 0, Age Class C CI:", ci_preg_0_c, "\n")
 cat("Pregnant = 1, Age Class C CI:", ci_preg_1_c, "\n")
 
 cat("Foal = 0, Age Class A CI:", ci_foal_0_a, "\n")
 cat("Foal = 1, Age Class A CI:", ci_foal_1_a, "\n")
 cat("Foal = 0, Age Class B CI:", ci_foal_0_b, "\n")
 cat("Foal = 1, Age Class B CI:", ci_foal_1_b, "\n")
 cat("Foal = 0, Age Class C CI:", ci_foal_0_c, "\n")
 cat("Foal = 1, Age Class C CI:", ci_foal_1_c, "\n")
 
 
 # attempt 7 ----
 # Model estimates
 intercept <- -1.82976
 intercept_diff_b <- -0.39465
 intercept_diff_c <- -0.38680
 pregnant_effect_a <- -2.14431
 pregnant_effect_b <- -2.14431 + 1.05973
 pregnant_effect_c <- -2.14431 + 0.90357
 foal_effect_a <- -1.50365
 foal_effect_b <- -1.50365 + 0.99484
 foal_effect_c <- -1.50365 + 1.10578
 
 # Calculate probabilities
 prob_preg_0_a <- plogis(intercept)
 prob_preg_0_b <- plogis(intercept + intercept_diff_b)
 prob_preg_0_c <- plogis(intercept + intercept_diff_c)
 prob_preg_1_a <- plogis(intercept + pregnant_effect_a)
 prob_preg_1_b <- plogis(intercept + intercept_diff_b + pregnant_effect_b)
 prob_preg_1_c <- plogis(intercept + intercept_diff_c + pregnant_effect_c)
 
 prob_foal_0_a <- plogis(intercept)
 prob_foal_0_b <- plogis(intercept + intercept_diff_b)
 prob_foal_0_c <- plogis(intercept + intercept_diff_c)
 prob_foal_1_a <- plogis(intercept + foal_effect_a)
 prob_foal_1_b <- plogis(intercept + intercept_diff_b + foal_effect_b)
 prob_foal_1_c <- plogis(intercept + intercept_diff_c + foal_effect_c)
 
 # Calculate standard errors
 se_intercept <- 0.23775
 se_intercept_diff_b <- 0.10833
 se_intercept_diff_c <- 0.13911
 se_pregnant_effect_a <- 0.23879
 se_pregnant_effect_b <- 0.27479
 se_pregnant_effect_c <- 0.35580
 se_foal_effect_a <- 0.31173
 se_foal_effect_b <- 0.33162
 se_foal_effect_c <- 0.37296
 
 # Adjust standard errors for pregnant probabilities
 adjusted_se_preg_0_a <- sqrt((se_intercept * prob_preg_0_a * (1 - prob_preg_0_a))^2)
 adjusted_se_preg_0_b <- sqrt((se_intercept * prob_preg_0_b * (1 - prob_preg_0_b))^2 + (se_intercept_diff_b * prob_preg_0_b * (1 - prob_preg_0_b))^2)
 adjusted_se_preg_0_c <- sqrt((se_intercept * prob_preg_0_c * (1 - prob_preg_0_c))^2 + (se_intercept_diff_c * prob_preg_0_c * (1 - prob_preg_0_c))^2)
 adjusted_se_preg_1_a <- sqrt((se_intercept * prob_preg_1_a * (1 - prob_preg_1_a))^2 + (se_pregnant_effect_a * prob_preg_1_a * (1 - prob_preg_1_a))^2)
 adjusted_se_preg_1_b <- sqrt((se_intercept * prob_preg_1_b * (1 - prob_preg_1_b))^2 + (se_intercept_diff_b * prob_preg_1_b * (1 - prob_preg_1_b))^2 + (se_pregnant_effect_b * prob_preg_1_b * (1 - prob_preg_1_b))^2)
 adjusted_se_preg_1_c <- sqrt((se_intercept * prob_preg_1_c * (1 - prob_preg_1_c))^2 + (se_intercept_diff_c * prob_preg_1_c * (1 - prob_preg_1_c))^2 + (se_pregnant_effect_c * prob_preg_1_c * (1 - prob_preg_1_c))^2)
 
 # Adjust standard errors for foal probabilities
 adjusted_se_foal_0_a <- sqrt((se_intercept * prob_foal_0_a * (1 - prob_foal_0_a))^2)
 adjusted_se_foal_0_b <- sqrt((se_intercept * prob_foal_0_b * (1 - prob_foal_0_b))^2 + (se_intercept_diff_b * prob_foal_0_b * (1 - prob_foal_0_b))^2)
 adjusted_se_foal_0_c <- sqrt((se_intercept * prob_foal_0_c * (1 - prob_foal_0_c))^2 + (se_intercept_diff_c * prob_foal_0_c * (1 - prob_foal_0_c))^2)
 adjusted_se_foal_1_a <- sqrt((se_intercept * prob_foal_1_a * (1 - prob_foal_1_a))^2)
 adjusted_se_foal_1_b <- sqrt((se_intercept * prob_foal_1_b * (1 - prob_foal_1_b))^2 + (se_intercept_diff_b * prob_foal_1_b * (1 - prob_foal_1_b))^2 + (se_foal_effect_b * prob_foal_1_b * (1 - prob_foal_1_b))^2)
 adjusted_se_foal_1_c <- sqrt((se_intercept * prob_foal_1_c * (1 - prob_foal_1_c))^2 + (se_intercept_diff_c * prob_foal_1_c * (1 - prob_foal_1_c))^2 + (se_foal_effect_c * prob_foal_1_c * (1 - prob_foal_1_c))^2)
 
 # Calculate ci for pregnant probabilities
 ci_preg_0_a <- c(prob_preg_0_a - z * adjusted_se_preg_0_a, prob_preg_0_a + z * adjusted_se_preg_0_a)
 ci_preg_0_b <- c(prob_preg_0_b - z * adjusted_se_preg_0_b, prob_preg_0_b + z * adjusted_se_preg_0_b)
 ci_preg_0_c <- c(prob_preg_0_c - z * adjusted_se_preg_0_c, prob_preg_0_c + z * adjusted_se_preg_0_c)
 ci_preg_1_a <- c(prob_preg_1_a - z * adjusted_se_preg_1_a, prob_preg_1_a + z * adjusted_se_preg_1_a)
 ci_preg_1_b <- c(prob_preg_1_b - z * adjusted_se_preg_1_b, prob_preg_1_b + z * adjusted_se_preg_1_b)
 ci_preg_1_c <- c(prob_preg_1_c - z * adjusted_se_preg_1_c, prob_preg_1_c + z * adjusted_se_preg_1_c)
 
 # Calculate ci for foal probabilities
 ci_foal_0_a <- c(prob_foal_0_a - z * adjusted_se_foal_0_a, prob_foal_0_a + z * adjusted_se_foal_0_a)
 ci_foal_0_b <- c(prob_foal_0_b - z * adjusted_se_foal_0_b, prob_foal_0_b + z * adjusted_se_foal_0_b)
 ci_foal_0_c <- c(prob_foal_0_c - z * adjusted_se_foal_0_c, prob_foal_0_c + z * adjusted_se_foal_0_c)
 ci_foal_1_a <- c(prob_foal_1_a - z * adjusted_se_foal_1_a, prob_foal_1_a + z * adjusted_se_foal_1_a)
 ci_foal_1_b <- c(prob_foal_1_b - z * adjusted_se_foal_1_b, prob_foal_1_b + z * adjusted_se_foal_1_b)
 ci_foal_1_c <- c(prob_foal_1_c - z * adjusted_se_foal_1_c, prob_foal_1_c + z * adjusted_se_foal_1_c)
 
 # show
 cat("Pregnant = 0, Age Class A CI:", ci_preg_0_a, "\n")
 cat("Pregnant = 1, Age Class A CI:", ci_preg_1_a, "\n")
 cat("Pregnant = 0, Age Class B CI:", ci_preg_0_b, "\n")
 cat("Pregnant = 1, Age Class B CI:", ci_preg_1_b, "\n")
 cat("Pregnant = 0, Age Class C CI:", ci_preg_0_c, "\n")
 cat("Pregnant = 1, Age Class C CI:", ci_preg_1_c, "\n")
 
 cat("Foal = 0, Age Class A CI:", ci_foal_0_a, "\n")
 cat("Foal = 1, Age Class A CI:", ci_foal_1_a, "\n")
 cat("Foal = 0, Age Class B CI:", ci_foal_0_b, "\n")
 cat("Foal = 1, Age Class B CI:", ci_foal_1_b, "\n")
 cat("Foal = 0, Age Class C CI:", ci_foal_0_c, "\n")
 cat("Foal = 1, Age Class C CI:", ci_foal_1_c, "\n")

 
# tables for model outputs ----

 # extract  z-values& p-values
 coef_summary <- fixef(model2_interactions_foal_pregnant)
 se_summary <- sqrt(diag(vcov(model2_interactions_foal_pregnant)))
 z_values <- coef_summary / se_summary
 p_values <- 2 * (1 - pnorm(abs(z_values)))
 
 p_value_format <- ifelse(p_values < 0.001, "<0.001", sprintf("%.3f", p_values))
 
 
 # Combine coefficients, standard errors, z-values, and p-values into a data frame
 coef_table <- data.frame(Estimate = coef_summary, SE = se_summary, "z_value" = z_values, "p_value" = p_value_format)
 
 # Round coefficients and standard errors
 coef_table$Estimate <- round(coef_table$Estimate, digits = 3)
 coef_table$SE <- round(coef_table$SE, digits = 3)
 coef_table$z_value <- round(coef_table$z_value, digits = 3)
 coef_table$p_value <- (coef_table$p_value)
   #round(coef_table$p_value, digits = 4)
 
 # table
 kable(coef_table, format = "html") %>%
   kable_styling(full_width = FALSE) %>%
   scroll_box(width = "100%", height = "400px")
 
 # rounding function
 round_to_4_digits <- function(x) {
   round(x, digits = 4)
 }
 
 # extract model info
 AIC_val <- AIC(model2_interactions_foal_pregnant)
 BIC_val <- BIC(model2_interactions_foal_pregnant)
 logLik_val <- logLik(model2_interactions_foal_pregnant)
 deviance_val <- deviance(model2_interactions_foal_pregnant)
 df_resid_val <- df.residual(model2_interactions_foal_pregnant)
 
 # df for model information
 model_info_df <- data.frame(
   AIC = round_to_4_digits(AIC_val),
   BIC = round_to_4_digits(BIC_val),
   logLik = round_to_4_digits(logLik_val),
   deviance = round_to_4_digits(deviance_val),
   df.resid = df_resid_val
 )
 
 # df for scaled residuals
 scaled_residuals_summary <- summary(model2_interactions_foal_pregnant)$residuals
 scaled_residuals_df <- data.frame(
   Min = round_to_4_digits(min(scaled_residuals_summary)),
   Q1 = round_to_4_digits(quantile(scaled_residuals_summary, probs = 0.25)),
   Median = round_to_4_digits(median(scaled_residuals_summary)),
   Q3 = round_to_4_digits(quantile(scaled_residuals_summary, probs = 0.75)),
   Max = round_to_4_digits(max(scaled_residuals_summary))
 )
 
 # df for random effects
 random_effects_summary <- summary(model2_interactions_foal_pregnant)$varcor
 random_effects_df <- data.frame(
   Groups = rep(names(random_effects_summary), each = nrow(random_effects_summary[[1]])),
   Name = rep(rownames(random_effects_summary[[1]]), times = length(random_effects_summary)),
   Variance = round_to_4_digits(unlist(lapply(random_effects_summary, function(x) x[[1]]))),
   Std.Dev = round_to_4_digits(unlist(lapply(random_effects_summary, function(x) sqrt(x[[1]]))))
 )
 
 # make tables
 model_info_table <- kable(model_info_df, format = "html") %>%
   kable_styling(full_width = FALSE) %>%
   scroll_box(width = "100%", height = "200px")
 
 scaled_residuals_table <- kable(scaled_residuals_df, format = "html") %>%
   kable_styling(full_width = FALSE) %>%
   scroll_box(width = "100%", height = "200px")
 
 random_effects_table <- kable(random_effects_df, format = "html") %>%
   kable_styling(full_width = FALSE) %>%
   scroll_box(width = "100%", height = "200px")
 
 # show
 model_info_table
 scaled_residuals_table
 random_effects_table
# plots ----
# 1 - pregnant-----

# 2 plots with actual predictions, but they do 1 line only 
 (plot_foalalive <- ggplot(for_model, aes(x = foal_alive, y = change, color = as.factor(age_class))) +
    geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE) +
    scale_color_manual(values = c("#BF3EFF", "#4ecb50", "#FF8C00"),
                       labels = c("young", "prime-aged", "old"))+
    theme_classic() +
    ylab("Probability of leaving current harem\n") + 
   # scale_x_continuous(breaks=c(0,1), labels = c("Not pregnant", "Pregnant"))+
    labs(color = "age class\n") +
    scale_y_continuous()+
    theme(axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 15),
          axis.title.y = element_text(size = 15, face = "plain"), 
          axis.title.x =element_blank(),
          panel.grid = element_blank(), 
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 12),
          legend.position = "bottom",
          legend.background = element_rect(colour = "grey")))

# make prediction data
prediction_data <- expand.grid(pregnant = c(0, 1),
                               age_class = c("a", "b", "c"))

# set stuff to reference level like in the summary output
prediction_data$time_last_observed_scaled <- 0  
prediction_data$foal_alive <- 0  
prediction_data$prev_haremsize_scaled <- 0

#  predictions 
pred <- predict(model2_interactions_foal_pregnant, newdata = prediction_data, type = "response", re.form = NA)

prediction_data$predicted <- pred

(final_plot <- ggplot(prediction_data, aes(x = pregnant, y = predicted, color = as.factor(age_class))) +
    geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE) +
    scale_color_manual(values = c("#BF3EFF", "#4ecb50", "#FF8C00"),
                       labels = c("younger than 4", "4-10", "older than 10"))+
    theme_classic() +
    scale_x_continuous(breaks=c(0,1), 
                       limits = c(0,1.1),
                       labels = c("Not pregnant", "Pregnant"))+
    scale_y_continuous(limits = c(0,0.15))+
    ylab("Probability of leaving current harem\n") +                             
    labs(color = "age class\n") +
    ggtitle("a)\n")+
    theme(axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          axis.title.y = element_text(size = 12), 
          axis.title.x =element_blank(),
          panel.grid = element_blank(), 
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 12),
          legend.position = "none"
          ,legend.background = element_rect(colour = "grey")
        ))


# 2 - foal-----

fit2 <- as.data.frame(effect('with_foal', interactions_foal_pregnant))
(foalplot <- ggplot(for_model, aes(x = with_foal, y = change)) +
  geom_line(data = fit2, aes(y = fit2, linetype = "model predictions"), size = 1, col = "#006400") +
  scale_y_continuous(limits = c(0, 0.15)) +
  labs(y = "Probability of changing groups\n", x = "Foal\n") +
  theme_classic() +
  theme(
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 12, face = "plain"),
    panel.grid = element_blank(), 
    legend.text = element_text(size = 10),
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    legend.title = element_blank(),
    legend.box.background = element_rect(colour = "darkgrey")) +
  scale_linetype_manual('', values = c("model predictions" = 1)) +
  scale_size_area())
  
(foal_smooth <- ggplot(for_model, aes(x = foal_alive, y = change, color = as.factor(age_class))) +
      geom_smooth(method = "glm", method.args = list(family = "binomial")) +
      scale_color_manual(values = c("#BF3EFF", "#4ecb50", "#FF8C00"),
                         labels = c("young", "prime-aged", "old"))+
      theme_classic() +
      ylab("Probability of leaving current harem\n") +                             
    scale_x_continuous(breaks=c(0,1), limits = c(0,1.1), labels = c("No foal", "Foal"))+
      labs(color = "age class\n") +
      scale_y_continuous()+
      theme(axis.text.x = element_text(size = 12),
            axis.text.y = element_text(size = 15),
            axis.title.y = element_text(size = 15, face = "plain"), 
            axis.title.x =element_blank(),
            panel.grid = element_blank(), 
            legend.text = element_text(size = 12),
            legend.title = element_text(size = 12),
            legend.position = "bottom",
            legend.background = element_rect(colour = "grey")))

prediction_data2 <- expand.grid(with_foal = c(0, 1),
                                age_class = c("a", "b", "c"))
prediction_data2$time_last_observed_scaled <- 0  
prediction_data2$pregnant <- 0  
prediction_data2$prev_haremsize_scaled <- 0
# Calculate predictions including random effects
pred2 <- predict(interactions_foal_pregnant, newdata = prediction_data2, type = "response", re.form = NA)
# Combine predictions with prediction data
prediction_data2$predicted <- pred2

(plot_foal <- ggplot(prediction_data2, aes(x = with_foal, y = predicted, color = as.factor(age_class))) +
    geom_smooth(method = "glm", method.args = list(family = "binomial")) +
    scale_color_manual(values = c("#BF3EFF", "#4ecb50", "#FF8C00"),
                       labels = c("young", "prime-aged", "old"))+
    theme_classic() +
    ggtitle("b)\n")+
    scale_x_continuous(breaks=c(0,1), limits=c(0,1.1), labels = c("No foal", "Foal"))+
    scale_y_continuous(limits = c (0, 0.15))+
    ylab("Probability of leaving current harem\n") +                             
    labs(color = "age class\n") +
    theme(axis.text.x = element_text(size = 12),
          axis.text.y = element_blank(),
          axis.title.y = element_blank(), 
          axis.title.x =element_blank(),
         # legend.text = element_text(size = 10),
         # legend.title = element_text(size = 10),
          legend.position = "none",
         # legend.background = element_rect(colour = "grey")
         ))

plot_foal
  
ggsave("przewalski/foalplot.png", width = 7, height = 6, dpi = 300)
# this has worked before but for some reason now stopped working. Resort to using geom_smooth and not model predictions.
prediction_data3 <- expand.grid(foal_alive = c(0, 1),
                                age_class = c("a", "b", "c"))
prediction_data3$time_last_observed_scaled <- 0  
prediction_data3$pregnant <- 0  
prediction_data3$prev_haremsize_scaled <- 0
# Calculate predictions including random effects
pred3 <- predict(model2_interactions_foal_pregnant, newdata = prediction_data3, type = "response", re.form = NA)
# Combine predictions with prediction data
prediction_data3$predicted <- pred3

(plot_foal_alive <- ggplot(prediction_data3, aes(x = foal_alive, y = predicted, color = as.factor(age_class))) +
    geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE) +
    scale_color_manual(values = c("#BF3EFF", "#4ecb50", "#FF8C00"),
                       labels = c("young", "prime-aged", "old"))+
    theme_classic() +
    scale_x_continuous(breaks=c(0,1), labels = c("No foal", "Foal"))+
    scale_y_continuous(limits = c (0, 0.12))+
    #  scale_y_continuous(breaks = seq(0, 1, by = 0.05), limits = c(0.01, 0.19)) +
    ylab("Probability of leaving current harem\n") +                             
    labs(color = "age class\n") +
    theme(axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 15),
          axis.title.y = element_text(size = 15, face = "plain"), 
          axis.title.x =element_blank(),
          panel.grid = element_blank(), 
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 12),
          legend.position = "bottom",
          legend.background = element_rect(colour = "grey")))
ggsave("przewalski/foalalive.png", width = 7, height = 6, dpi = 300)

# 3 - harem size and time-----
predictionsharem <- ggpredict(ageclass2_interactions_all2, terms = c("previous_haremsize"))

(plot_effects <- plot(predictionsharem))

pred.harem <- ggpredict(interactions_foal_pregnant, terms = c("previous_haremsize"))  # this gives overall predictions for the model

ci_lower_logodds <- pred.harem$predicted - qnorm(0.975) * sqrt(0.01889)
ci_upper_logodds <- pred.harem$predicted + qnorm(0.975) * sqrt(0.01889)
# Back-transform to the probability scale
ci_lower_prob <- plogis(ci_lower_logodds)
ci_upper_prob <- plogis(ci_upper_logodds)

# Create a data frame for plotting
ci_data <- data.frame(x = pred.harem$x, ci_lower = ci_lower_prob, ci_upper = ci_upper_prob)

(haremplot <- ggplot(pred.harem) +
    geom_line(aes(x = x, y = predicted), 
                  #shape = "model predictions", linetype = "model predictions"),
              size = 0.7,
              color = "#006400") + 
    stat_smooth(aes(x = x, y = predicted),method = "glm", method.args = list(family = "binomial"), 
                formula = y ~ x, se = TRUE, fill = "lightblue", alpha = 0.3, geom = "ribbon") +
    scale_linetype_manual('', values =c("model predictions" = 1 ))+
    scale_y_continuous (limits = c(0,0.15))+
   # geom_ribbon(aes(ymin = predicted - 1.96 * 0.01889, 
    #                ymax = predicted + 1.96 * 0.01889, 
    #            x = x),
    #            fill = "lightblue", alpha = 0.3) + # 95% confidence interval
    theme_classic() +
  #  ggtitle("c)\n")+
    ylab("Probability of leaving current harem\n") +                             
    xlab("\nHarem size (adult females)")  +
    theme(axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          axis.title = element_text(size = 12),                      
          panel.grid = element_blank(), 
          legend.position = "none"
          ))
ggsave("przewalski/haremplot.png", width = 7, height = 5, dpi = 300)
haremplot2
# time
predictionstime <- ggpredict(model2_interaction_pregnant, terms="time_last_observed_scaled [all]" )

(plot_effects2 <- plot(predictionstime))



(timeplot <- ggplot(predictionstime) +
    geom_line(aes(x = x, y = predicted, shape = "model predictions", linetype = "model predictions"),
              size = 0.7) + 
    scale_linetype_manual('', values =c("model predictions" = 1 ))+
    theme_classic() +
    ylab("Probability of leaving current harem\n") +                             
    xlab("\ntime since last observation")  +
    theme(axis.text.x = element_text(size = 15),
          axis.text.y = element_text(size = 15),
          axis.title = element_text(size = 15, face = "plain"),                      
          panel.grid = element_blank(), 
          legend.text=element_text(size = 12),
          legend.title = element_text(size = 12),
          legend.position = "bottom",
          legend.background = element_rect(colour = "grey")))
# back-scale x axis? 
# ask gpt tomorrow

# Undo scaling operation
predictionstime$time_last_observed_original <- predictionstime$x * sd(for_model$time_to_last_obs) + mean(for_model$time_to_last_obs)

# Calculate values for x-axis in months
predictionstime$months_since_last_observation <- predictionstime$time_last_observed_original / 30  # Assuming each unit is a day


(timeplot <- ggplot(predictionstime) +
    scale_linetype_manual('', values = c("model predictions" = 1 )) +
    geom_smooth(aes(x = months_since_last_observation, y = predicted,
                    shape = "model predictions", linetype = "model predictions"),
                method = "loess", se = FALSE,
                color = "#006400", size = 0.7) + 
    geom_ribbon(aes(ymin = pnorm(-1.96, mean = 0.28916, sd = 0.02836) * sd(for_model$time_to_last_obs) + mean(for_model$time_to_last_obs),
                    ymax = pnorm(1.96, mean = 0.28916, sd = 0.02836) * sd(for_model$time_to_last_obs) + mean(for_model$time_to_last_obs),
                    x = x), 
                fill = "lightgray", alpha = 0.3) + # 95% confidence interval
    theme_classic() +
    ylab("Probability of leaving current harem\n") +                             
    xlab("\nMonths since last observation") +
  #  ggtitle("d)\n")+
    scale_x_continuous(breaks = seq(0, 30, by = 5),
                       labels = seq(0, 30, by = 5)) +
    scale_y_continuous(breaks = c(0,0.5,1))+
    theme(axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_text(size = 12),
          axis.title.y=element_blank(),
          panel.grid = element_blank(), 
          legend.text = element_text(size = 10),
          legend.title = element_text(size = 10),
          legend.position = "right",
         legend.background = element_rect(colour = "grey")
        ))
ggsave("przewalski/timeplot.png", width = 7, height = 5, dpi = 300)

library(gridExtra)

f_p_2 <- grid.arrange(final_plot, plot_foal_alive, ncol = 2)
ggsave("combined_plot.png", plot = grid.draw(f_p_2), width = 10, height = 5)

(f_p_2 <- final_plot+plot_foal_alive)

# Arrange the plots in a 2x2 grid
(foal_and_p <- plot_grid(final_plot, plot_foal_alive,ncol = 2, heights =c(2,1)))
ggsave("przewalski/combined_plots_v1.png", width = 10, height = 8, dpi = 300)

(pregnant_and_foal <- final_plot + plot_foal + plot_layout(ncol = 2))
ggsave("przewalski/foal_and_p_2.png",width = 10, height = 5, dpi = 300 )



# fails ----

# trying to get ci-s
# Install and load the 'effects' package if you haven't already
install.packages("effects")
library(effects)
summary(model2_interactions_foal_pregnant)

# Load the effects package
library(effects)

# Get the effects using the allEffects function
model_effects <- allEffects(model2_interactions_foal_pregnant)

# Print the model effects
summary(model_effects)

 # Get predicted probabilities
   predicted_probabilities <- predictions$fit

# Obtain predicted probabilities and confidence intervals using the 'allEffects()' function
   effects <- allEffects(model2_interactions_foal_pregnant)

 # Print the effects to see a summary
   summary(effects)

# Extract predicted probabilities and confidence intervals
predictions <- predict(model2_interactions_foal_pregnant, type = "response", se.fit = TRUE)

# Get predicted probabilities
predicted_probabilities <- predictions$fit

# Get standard errors
standard_errors <- predictions$se.fit

# Calculate confidence intervals
z_value <- qnorm(0.975)  # Z-score for 95% confidence interval
lower_ci <- predicted_probabilities - z_value * standard_errors
upper_ci <- predicted_probabilities + z_value * standard_errors

# Combine results into a data frame
result <- data.frame(
  predicted_probabilities = predicted_probabilities,
  lower_ci = lower_ci,
  upper_ci = upper_ci
)

# Show the result
print(result)


library(broom.mixed)
tidy(model2_interactions_foal_pregnant,conf.int=TRUE,exponentiate=TRUE,effects="fixed")

confint(model2_interactions_foal_pregnant, method = "boot")



# Get the fixed effects coefficients
fixed_effects <- fixef(model2_interaction_pregnant)

# Compute probabilities using the logistic function
probabilities <- plogis(fixed_effects)

# Get the confidence intervals for the fixed effects coefficients
ci <- confint(model2_interaction_pregnant)

# Compute confidence intervals for probabilities
ci_probabilities <- plogis(ci)

# Display the results
result <- data.frame(
  Variable = names(fixed_effects),
  Probability_Estimate = probabilities,
  Lower_CI = ci_probabilities[, "2.5 %"],
  Upper_CI = ci_probabilities[, "97.5 %"]
)

print(result)


# Create the dataset
df <- data.frame(
  age_class = c("a", "a", "b", "b", "c", "c"),
  pregnant = c(0, 1, 0, 1, 0, 1),
  probability = c(0.1383, 0.0184, 0.0976, 0.0353, 0.0983, 0.0305),
  CI_lower = c(0.08274558, 0.006489685, 0.05248759, 0.009981388, 0.05043089, 0.004430796),
  CI_upper = c(0.1937882, 0.03041026, 0.142672, 0.06054674, 0.1461156, 0.05667301)
)

(pregnant_barplot <- ggplot(df, aes(x = age_class, y = probability, fill = factor(pregnant))) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), color = "black") +
  geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper), 
                position = position_dodge(width = 0.9), 
                width = 0.2, 
                size = 0.4) +
    ggtitle("a)\n")+
    scale_fill_manual(values = c("#3CB371", "#9ACD32"),  labels = c("not pregnant", "pregnant")) +
    scale_x_discrete(labels = c("a" = " younger than 4", "b" = "4-10", "c" = " older than 10")) +
  theme_classic() +
  labs(x = "\nAge Class", y = "Probability of changing harems\n") +
  theme(legend.position = "bottom",
        legend.title = (element_blank()),
        axis.title.y = element_text(size = 12), 
        axis.title.x =element_blank(),
        legend.text = element_text(size = 10),
        legend.background = element_rect(colour = "grey"),
                          axis.text = element_text(size = 11)))

ggsave("przewalski/pregnant_plot_2.png", height = 7, width = 7, dpi = 300)

df2 <- data.frame(
  age_class = c("a", "a", "b", "b", "c", "c"),
  foal_alive = c(0, 1, 0, 1, 0, 1),
  probability = c(0.1383, 0.0344, 0.0976, 0.061, 0.0983, 0.0682),
  CI_lower = c(0.08274558, 0.0189458, 0.05248759, 0.01361585, 0.05043089, 0.0104536),
  CI_upper = c(0.1937882, 0.04993949, 0.142672, 0.1084668, 0.1461156, 0.1259774)
)
(foal_barplot <- ggplot(df2, aes(x = age_class, y = probability, fill = factor(foal_alive))) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.9), color = "black") +
    geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper), 
                  position = position_dodge(width = 0.9), 
                  width = 0.2, 
                  size = 0.4) +
    ggtitle("b)\n")+
    scale_fill_manual(values = c("#EE7600", "#FFC125"),  labels = c("no foal", "foal present")) +
    scale_x_discrete(labels = c("a" = " younger than 4", "b" = "4-10", "c" = " older than 10")) +
    theme_classic() +
    labs(x = "\nAge Class") +
    theme(legend.position = "bottom",
          legend.title = (element_blank()),
          axis.title.y = element_blank(), 
          axis.title.x =element_blank(),
          axis.text.y= element_blank(),
          legend.text = element_text(size = 10),
          legend.background = element_rect(colour = "grey"),
          axis.text = element_text(size = 11)))


ggsave("przewalski/foalplot_2.png", height = 7, width = 7, dpi = 300)

# 4 plots - 2nd version
(combined_plot_2 <- pregnant_barplot+ foal_barplot+ haremplot+timeplot +
    plot_layout(ncol = 2))
ggsave("przewalski/combined_plots_v2.png", width = 10, height = 8, dpi = 300)

# 2 plots
(p_and_f_bar <- pregnant_barplot + foal_barplot+
    plot_layout(ncol = 2))
ggsave("przewalski/P_and_f_barplot.png", width = 10, height = 5, dpi = 300)

# random plots for appendix----
observation_counts <- table(for_model$age_class)

# Convert observation counts to data frame
observation_data <- data.frame(age_class = names(observation_counts),
                               count = as.numeric(observation_counts))

# Create the bar plot
(ageplot <- ggplot(observation_data, aes(x = age_class, y = count)) +
  geom_bar(stat = "identity", fill = "#9ACD32", color = "black") +
    scale_x_discrete(  labels = c("< 4", "4-10", "> 10")) +
    geom_text(aes(label = paste("n =", count)), vjust = -0.5, size = 4, color = "black") +
  labs(x = "\nAge",
       y = "Observation Count\n")+
    ggtitle("a)\n")+
    theme_classic()+
    theme(axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10)))

# histogram of harem sizes

bw <- 2 * IQR(for_model$harem_size) / length(for_model$harem_size)^(1/3)

(haremsize_hist <- ggplot(for_model, aes ( x = harem_size))+
  geom_histogram(bins = 35, colour = "#006400" , fill= "#9ACD32", binwidth = 1.02
                 )+
    labs(x = "\n Adult harem size",
         y = "Observation count\n" )+
    ggtitle("b)\n")+
    theme(axis.text.y = element_text(size = 10),
            axis.text.x = element_text(size = 10))+
  theme_classic())

pregnant_counts <- table(for_model$pregnant)
pregnant_data <- data.frame(pregnant = names(pregnant_counts),
                            count = as.numeric(pregnant_counts))
(pregnant_plot_desc <- ggplot(pregnant_data, aes(x = pregnant, y = count)) +
    geom_bar(stat = "identity", fill = "#9ACD32", color = "black") +
    geom_text(aes(label = paste("n =", count)), vjust = -0.5, size = 4, color = "black") +
    labs( y = "Observation Count\n")+
    ggtitle("c)\n")+
    scale_x_discrete(labels = c("0" = " not pregnant", "1" = "pregnant")) +
    theme_classic()+
    theme(axis.title.x = element_blank(),
      axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10)))

foal_counts <- table(for_model$foal_alive)
foal_data <- data.frame(foal = names(foal_counts),
                            count = as.numeric(foal_counts))
(foal_plot_desc <- ggplot(foal_data, aes(x = foal, y = count)) +
    geom_bar(stat = "identity", fill = "#9ACD32", color = "black") +
    geom_text(aes(label = paste("n =", count)), vjust = -0.5, size = 4, color = "black") +
    scale_x_discrete(labels = c("0" = "no foal", "1" = "with foal")) +
    labs(y = "Observation Count\n")+
    ggtitle("d)\n")+
    theme_classic()+
    theme(axis.title.x = element_blank(),
      axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10)))


(descriptive_plot <- ageplot + haremsize_hist + pregnant_plot_desc + foal_plot_desc)

ggsave("przewalski/appendix_plot.png", width = 11, height = 10, dpi = 300)

for_model <- for_model %>%  mutate(age_years = age/365)





changes<- subset(for_model, change == 1)
nrow(changes)
summary_changes <- changes %>% 
  group_by(age_class) %>% 
  summarise(number_of_changes = length(name))
summary_changes


# Create the new dataset
df_effects_p <- data.frame(
  age_class = c("a", "a", "b", "b", "c", "c"),
  pregnant = c(0, 1, 0, 1, 0, 1),
  probability = c(0.1383, 0.0184, 0.0976, 0.0353, 0.0983, 0.0305),
  CI_lower = c(0.088314028, 0.009531552, 0.063485402, 0.021295845, 0.062373336, 0.015659916),
  CI_upper = c(0.212177311, 0.035325734, 0.147531359, 0.058039446, 0.151910588, 0.058651393)
)

# Create the plot
(pregnant_effects_plot <- ggplot(df_effects_p, aes(x = age_class, y = probability, fill = factor(pregnant))) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), color = "black") +
  geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper), 
                position = position_dodge(width = 0.9), 
                width = 0.2, 
                size = 0.4) +
  ggtitle("a)\n")+
  scale_fill_manual(values = c("#3CB371", "#9ACD32"),  labels = c("not pregnant", "pregnant")) +
  scale_x_discrete(labels = c("a" = " younger than 4", "b" = "4-10", "c" = " older than 10")) +
  theme_classic() +
  labs(x = "\nAge Class", y = "Probability of leaving current harem\n") +
  theme(legend.position = "bottom",
        legend.title = (element_blank()),
        axis.title.y = element_text(size = 12), 
        axis.title.x =element_blank(),
        legend.text = element_text(size = 10),
        legend.background = element_rect(colour = "grey"),
        axis.text = element_text(size = 11)))

df_effect_f <- data.frame(
  age_class = c("a", "a", "b", "b", "c", "c"),
  foal_alive = c(0, 1, 0, 1, 0, 1),
  probability = c(0.1383, 0.0344, 0.0976, 0.061, 0.0983, 0.0682),
  CI_lower = c(0.088874919, 0.016206961, 0.063098724, 0.037473133, 0.061261913, 0.038536053),
  CI_upper = c(0.211615352, 0.072183917, 0.148732509, 0.098176816, 0.155084447, 0.118686106)
)
(foal_effects_plot <- ggplot(df_effect_f, aes(x = age_class, y = probability, fill = factor(foal_alive))) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.9), color = "black") +
    geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper), 
                  position = position_dodge(width = 0.9), 
                  width = 0.2, 
                  size = 0.4) +
    ggtitle("b)\n")+
    scale_fill_manual(values = c("#EE7600", "#FFC125"),  labels = c("no foal", "foal present")) +
    scale_x_discrete(labels = c("a" = " younger than 4", "b" = "4-10", "c" = " older than 10")) +
    theme_classic() +
    labs(x = "\nAge Class") +
    theme(legend.position = "bottom",
          legend.title = (element_blank()),
          axis.title.y = element_blank(), 
          axis.title.x =element_blank(),
          axis.text.y= element_blank(),
          legend.text = element_text(size = 10),
          legend.background = element_rect(colour = "grey"),
          axis.text = element_text(size = 11)))

(p_and_f_bar_effects <- pregnant_effects_plot + foal_effects_plot)
  #  plot_layout(ncol = 2))
ggsave("przewalski/P_and_f_effects_barplot.png", width = 10, height = 5, dpi = 300)
p_and_f_bar_effects

# cheating----
(plot_foal_alive <- ggplot(prediction_data3, aes(x = foal_alive, y = predicted, color = as.factor(age_class2))) +
   geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE) +
   scale_color_manual(values = c("#BF3EFF", "#4ecb50", "#FF8C00"),
                      labels = c("< 4", "4-10", "> 10"))+
   theme_classic() +
   ggtitle("b)\n")+
   scale_x_continuous(breaks=c(0,1), limits=c(0,1.1), labels = c("No foal", "Foal"))+
   scale_y_continuous(limits = c (0, 0.15))+
   ylab("Probability of leaving current harem\n") +                             
   labs(color = "age (years) \n") +
   theme(axis.text.x = element_text(size = 12),
         axis.text.y = element_blank(),
         axis.title.y = element_blank(), 
         axis.title.x =element_blank(),
          legend.text = element_text(size = 10),
          legend.title = element_text(size = 10),
         legend.position = "right",
          legend.background = element_rect(colour = "grey")
   ))
ggsave("przewalski/foalalive.png", width = 7, height = 6, dpi = 300)



# Calculate back-transformed and adjusted confidence intervals on the log odds scale
ci_lower_logodds <- 0.28916 - 1.96 * 0.02836
ci_upper_logodds <- 0.28916 + 1.96 * 0.02836

# Back-transform to original scale (probability)
ci_lower_prob <- plogis(ci_lower_logodds)
ci_upper_prob <- plogis(ci_upper_logodds)

# Convert to the scale of days
ci_lower_days <- ci_lower_prob * sd(for_model$time_to_last_obs) + mean(for_model$time_to_last_obs)
ci_upper_days <- ci_upper_prob * sd(for_model$time_to_last_obs) + mean(for_model$time_to_last_obs)

# Convert to the scale of months
ci_lower_months <- ci_lower_days / 30
ci_upper_months <- ci_upper_days / 30


timeplot_2 <- ggplot(predictionstime, aes(x = months_since_last_observation, y = predicted)) +
  geom_smooth(method = "loess",  color = "#006400", fill = "lightblue", size = 0.65) + 
  theme_classic() +
  ylab("Probability of being observed in a different harem\n") +                             
  xlab("\nMonths since last observation") +
  scale_x_continuous(breaks = seq(0, 30, by = 5), labels = seq(0, 30, by = 5)) +
  scale_y_continuous(breaks = c(0, 0.5, 1)) +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        panel.grid = element_blank(), 
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10),
        legend.position = "right",
        legend.background = element_rect(colour = "grey")
  )

timeplot_2
ggsave("przewalski/timeplot.png", width = 7, height = 5, dpi = 300)


# Calculate confidence intervals adjusted for binomial response
pred.harem$ci_lower_logodds <- pred.harem$predicted - 1.96 * sqrt(0.01889)
pred.harem$ci_upper_logodds <- pred.harem$predicted + 1.96 * sqrt(0.01889)

# Back-transform to the probability scale
pred.harem$ci_lower <- plogis(pred.harem$ci_lower_logodds)
pred.harem$ci_upper <- plogis(pred.harem$ci_upper_logodds)

# Plot the predicted line with adjusted confidence intervals
ggplot(pred.harem, aes(x = x, y = predicted)) +
  geom_line(size = 0.7, color = "#006400") +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), fill = "lightblue", alpha = 0.3) +
  scale_y_continuous(limits = c(0, 0.15)) +
  theme_classic() +
  ylab("Probability of leaving current harem\n") +
  xlab("\nHarem size (adult females)") +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 12),
        panel.grid = element_blank(),
        legend.position = "none")

# Plot the predicted line with confidence intervals using geom_smooth
ggplot(pred.harem, aes(x = x, y = predicted)) +
 # geom_line(size = 0.7, color = "#006400") +
  geom_smooth(aes(x = x, y = predicted), method = "glm", method.args = list(family = binomial(link = "logit")),  fill = "lightblue", alpha = 0.3) +
  scale_y_continuous(limits = c(0, 0.15)) +
  theme_classic() +
  ylab("Probability of leaving current harem\n") +
  xlab("\nHarem size (adult females)") +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 12),
        panel.grid = element_blank(),
        legend.position = "none")



