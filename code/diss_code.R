# code for my dissertation
# 2024 march - april
# packages----
library (tidyverse)
library (readxl)
library (writexl)
library (lme4)
library (MuMIn)
library (effects)
library (fortunes)

#----
# import datasets----
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
#----
# creating the variables of interest----
# 1 - time since last observation

total$date<-as.Date(total$date)
total<- total %>% 
  group_by(name) %>% 
  # dataset is already ordered by date
  mutate (last_obs = as.Date(lag(date)) ) %>%
  # last time focal horse appears in dataset
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
         year_birth = as.numeric(format(as.Date(dob_foal_prev_year, 
                                 format = "%Y-%m-%d"), "%Y")),
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
         year_birth = as.numeric(format(as.Date(dob_foal_next_year, 
                                format = "%Y-%m-%d"), "%Y")),
         year_observation = as.numeric(year_birth - 1)) %>% 
  select (-year_birth, -Name_mother, -Name, -Date_of_birth, -Name_father, - Date_of_death)

total_foals <- left_join (total_foals, foals_nextyear, by = c("name", "year_observation"))

write_xlsx (total_foals,"przewalski/data/total_foals.xlsx") # saving this dataset to check manually

# 6,7 - with foal and pregnant (explanatory variables)
# with foal: if female had a foal born from the harem stallion in year of or year before observation

total_foals <- total_foals %>% 
  mutate(with_foal = case_when(
    (!is.na(name_foal) & # foal born in year of obs
      dob_foal <= date & # foal born before observation
      father_of_foal == previous_harem) | # previous harem always refers to focal harem
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
                                  father_of_foal == previous_harem &
                                  (is.na(death_foal) | (!is.na(death_foal)& death_foal > date))| 
                                  (!is.na(foal_prev_year) & # foal born in year before obs
                                  father_of_foal_prev_year == previous_harem) &
                                  (is.na(death_foal_prev_year) | 
                                     (!is.na(death_foal_prev_year)  & death_foal_prev_year > date)) ~ 1,
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

# 11- age class


total_foals <- total_foals %>% 
  mutate(age_class = 
           case_when (age <= 1460 ~ "a" , # younger than 4
                      age  > 1460 & age <= 3650 ~ "b", # between 4 and 10
                      age > 3650 ~ "c") ) # older than 10


#-----
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

for_model$age_class <- as.factor(for_model$age_class)
for_model$year_observation <- as.factor(for_model$year_observation)

# scale time last observed for convergence
for_model$time_last_observed_scaled <- scale(for_model$time_to_last_obs)
for_model$time_last_observed_scaled <- as.numeric(for_model$time_last_observed_scaled) # variable was acting weird
for_model$prev_haremsize_scaled <- scale(for_model$previous_haremsize)
for_model$prev_haremsize_scaled <- as.numeric (for_model$prev_haremsize_scaled)

# define optimizer
control <- glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000))
#-----
# visualising raw data----
(plot <- ggplot(for_model, aes(x= with_foal, y = change))+geom_point()+geom_jitter())
(plot2 <- ggplot(for_model, aes(x= age_class, y = change))+geom_point()+geom_jitter())
(plot3 <- ggplot(for_model, aes(x= pregnant, y = change))+geom_point()+geom_jitter())
(plot4  <- ggplot(for_model, aes (x = foal_alive, y = change))+ geom_point()+geom_jitter())
#----
# models ----

# null model 
null_model <- glmer (change ~ 1+ (1|year_observation)  +(1|previous_harem/name), data = for_model, family = binomial)
AIC(null_model)

# models with the with_foal variable

# no interactions
simple_model <- glmer(change ~ time_last_observed_scaled + age_class + with_foal + pregnant +
                       prev_haremsize_scaled+ (1|year_observation)  + (1|previous_harem/name) ,  
                      data = for_model, family = binomial, control = control)
summary(simple_model)
# AIC 7770.8   
r2_simple <- r.squaredGLMM(simple_model)
r2_simple # 0.09 / 0.53 (m/c) - all models v similar R2 values.

# all interactions

all_interactions_model <- glmer (change ~ time_last_observed_scaled +age_class*pregnant + 
                                   age_class*with_foal + age_class*prev_haremsize_scaled+ 
                                   (1|year_observation)  +(1|previous_harem/name) ,  
                                 data = for_model, family = binomial, control = control)
summary(all_interactions_model) # aic 7761
r2_interactions_all <- r.squaredGLMM(all_interactions_model)
r2_interactions_all # 0.09 / 0.54

interactions_foal_pregnant <- glmer (change ~ time_last_observed_scaled +age_class*pregnant + 
                                       age_class*with_foal+  prev_haremsize_scaled+
                                       (1|year_observation)  +(1|previous_harem/name) ,  
                                     data = for_model, family = binomial, control = control)
summary (interactions_foal_pregnant) # AIC  7757.2  
r2_interactions_fp <- r.squaredGLMM(interactions_foal_pregnant)
r2_interactions_fp # 0.088 / 0.54


interaction_pregnant <- glmer (change ~ time_last_observed_scaled + age_class*pregnant +
                                with_foal + prev_haremsize_scaled +
                                (1|year_observation)  +(1|previous_harem/name),  
                               data = for_model, family = binomial, control = control)
summary(interaction_pregnant) # AIC 7762     
r2_interaction_p<- r.squaredGLMM(interaction_pregnant)
r2_interaction_p # 0.087 / 0.53

# no interactions
model2_simple <- glmer(change ~ time_last_observed_scaled+ age_class+ foal_alive +pregnant+ 
                         prev_haremsize_scaled +(1|year_observation)+ (1|previous_harem/name),  
                       data = for_model, family = binomial, control = control)
summary(model2_simple)
# AIC  7764.6
r2_simple <- r.squaredGLMM(model2_simple)
r2_simple # 0.09 / 0.53

# all interactions
all_interactions_model2 <- glmer (change ~ time_last_observed_scaled + age_class*pregnant +
                                    age_class*foal_alive + age_class*prev_haremsize_scaled+
                                    (1|year_observation)  +(1|previous_harem/name) ,  
                                  data = for_model, family = binomial, control = control)
summary(all_interactions_model2) # aic 7753.5 
r2_interactions_all <- r.squaredGLMM(all_interactions_model2)
r2_interactions_all # 0.09/0.54

# foal and pregnant interactions
model2_interactions_foal_pregnant<- glmer(change ~ time_last_observed_scaled + age_class*pregnant+
                                            age_class*foal_alive + prev_haremsize_scaled+ 
                                            (1|year_observation)  +(1|previous_harem/name) ,  
                                          data = for_model, family = binomial, control = control)
summary(model2_interactions_foal_pregnant) # aic 7749
r_squared_surviving <- r.squaredGLMM(model2_interactions_foal_pregnant) 
r_squared_surviving # 0.09/0.54


model2_interaction_pregnant <- glmer (change ~ time_last_observed_scaled + age_class*pregnant + 
                                        foal_alive + prev_haremsize_scaled +
                                        (1|year_observation)  +(1|previous_harem/name) ,  
                                      data = for_model, family = binomial, control = control)
summary (model2_interaction_pregnant) # aic 7755.7
r_squared_mod2p <- r.squaredGLMM(model2_interaction_pregnant) 
r_squared_mod2p # 0.089 /0.53

#----
# calculating probabilities from model output & ci-s----

# intercept: probability (p) of change = 1 when foal = 0 and pregnant = 0, 
# everything else at reference level
# i.e. p of change = 1 when foal = 0 and pregnant = 0 for age class a, 
# harem size and time last observed at their mean values (bc of scaling). 

plogis(-1.82976) 
# 0.1382669
# p of change = 1 for age class b when foal = 0 and pregnant = 0, everything else at ref level
plogis(-1.82976-0.39465 )
# 0.09757978
# p of change = 1 for age class c when foal = 0 and pregnant = 0, everything else at ref level
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

# confidence intervals based on effects package

model_effects <- allEffects(model2_interactions_foal_pregnant)

summary(model_effects)
#----