# 2024 march
# diss code but only the stuff i use

library (tidyverse)
library(readxl)
library (lme4)
library(MuMIn)
library(ggeffects)
library(effects)
library(sjPlot)   
library(insight)
library(httr)


# import data ----
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
nagylista <- read_excel("przewalski/data/nagylista.xls", 
                        col_types = c("text", "text", "text", 
                                      "text", "date", "text", "text", "text", 
                                      "text", "text", "text", "text", "date", 
                                      "text", "date", "text", "text", "text", 
                                      "text", "text", "text", "text", "text"))

# joining datasets, adding new variables----

# 1 - time since last observation
total$date<-as.Date(total$date)
total<- total %>% 
  group_by(name) %>% 
  # dataset is already ordered by date
  mutate (last_obs = as.Date(lag(date)) ) %>% 
  mutate(time_to_last_obs = as.numeric(date - last_obs)) %>% 
  ungroup()

# 2,3 -  harem stallion on previous observation, change/not (response var)
total <- total %>% 
  arrange(date) %>%
  group_by(name) %>%
  mutate(previous_harem = lag(harem)) %>%
  ungroup() %>% 
  group_by (date, name) %>% 
  mutate(change = case_when(
    harem == previous_harem ~ 0,
    harem != previous_harem ~ 1,
    is.na(previous_harem) ~ NA)) %>% 
  ungroup() 


# 4 - all the unique identifiers

nagylista$name<- as.character(nagylista$Name)
total_full <-dplyr::left_join(total, nagylista, by = c("name"))
# removing variables i dont need
total_full <- total_full %>%  select(-last_obs, -N, -St.b.Num., -Name, -Place_of_birth,  - Num_father, -Num_mother,-Number_of_Davis_case, -Branding, -Origin, 
         -`Known_date_(YMD)`, -`Natural(N)_or_Shoot(S)`,  -Chip, -Leavingdate, -Destination, - PZPtreatment_dateofprimer, -Note, -...23)

# 5 - age of horse & year of observation for next step

total_full <- total_full %>% 
  mutate_if(is.POSIXct, as.Date) %>% # making sure dates are read correctly
  mutate(year_observation = as.numeric(format(as.Date(date, format = "%Y-%m-%d"), "%Y")),
         age = as.numeric(date- Date_of_birth)) 

# joining foals to the dataset ( born this year, last year and next year compared to year of obs)
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

write_xlsx (total_foals,"przewalski/data/total_foals.xlsx")

# 6,7 - with foal and pregnant 
#  add foal not being dead at time of observation to with_foal category.
total_foals <- total_foals %>% 
  mutate(with_foal_4 = case_when(
    (!is.na(name_foal) & # foal born in year of obs
       dob_foal <= date & # foal born before observation
       father_of_foal == previous_harem) | 
      (!is.na(foal_prev_year) & # foal born in year before obs
         father_of_foal_prev_year == previous_harem) ~ 1,
    TRUE ~ 0
  ))
total_foals <- total_foals %>% 
  mutate(pregnant_3 = case_when(
    (!is.na(name_foal) &  # foal born in year of obs
       dob_foal > date &  # foal born after observation
       father_of_foal == previous_harem)  |
      (!is.na(foal_next_year) & 
         dob_foal_next_year - date < 330 &
         father_of_foal_next_year == previous_harem) ~ 1,  # Corrected logical condition
    TRUE ~ 0
  ))
# foal but alive
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
                                  TRUE ~ 0
  ))

# 9 - pzp yes or no 
pzp <- read_excel("przewalski/data/pzp/pzpdata.xlsx")

pzp <- pzp %>% mutate (pzp = "pzp",
                       name = Name)
pzp <- pzp %>%  select(name,Primer, pzp )

total_foals <- left_join(total_foals, pzp, by = c("name"))
total_foals <- total_foals %>% 
  mutate(pzp_treated = if_else(is.na(pzp), 0, if_else(pzp == "pzp" & Primer < date, 1, 0)))

# 10 - harem size

haremsize<- total_foals %>% 
  filter   (!age < 730,
            ! (harem == Name_father & age < (1095)),
            Gender == 2) %>% 
  group_by(date, harem) %>% 
  summarise(harem_size = length(name))
haremsize$date <- as.Date(haremsize$date)

total_foals_harem <- left_join(total_foals, haremsize, by= c("harem", "date"))

total_foals_harem <- total_foals_harem %>% 
  arrange(date) %>% 
  group_by(name) %>% 
  mutate(
    previous_haremsize =  lag(harem_size)) %>% 
  ungroup()

total_foals <- total_foals_harem

# age as a 4 level factor instead of continuous variable
total_foals <- total_foals %>% 
  mutate(age_class = 
           case_when (age < 1095 ~ "a" , # younger than 3
                      (age  >= 1095 & age <1825) ~ "b", # 3 to 5
                      age >= 1825 & age <= 3650 ~ "c", # 5 to 10
                      age > 3650 ~ "d")) # older than 10

total_foals <- total_foals %>% 
  mutate(age_class2 = 
           case_when (age <= 1460 ~ "a" , # younger than 4
                      age  > 1460 & age <= 3650 ~ "b", # between 4 and 10
                      age > 3650 ~ "c") ) # older than 10
str(total_foals$age_class2)

# subset for models ----
for_model <-  total_foals%>% 
  filter (Gender == 2 & age > 730) %>% 
  # only females older than 2
  filter(! (previous_harem == Name_father& age < 1095)) %>% 
  # post-dispersal females only 
  filter (date > as.Date("2008-01-01")) %>%
  # observations from 2008 onwards
  filter (!is.na(change),
          !is.na(age),
          !is.na(with_foal_4),
          !is.na(pregnant_3),
          !is.na(harem_size))
  # only keep data where there is data for all variables for models to be comparable
str(for_model)
for_model$age_class <- as.factor(for_model$age_class)
for_model$age_class2 <- as.factor(for_model$age_class2)
for_model$year_observation <- as.factor(for_model$year_observation)

# scale variables for convergence
for_model$age_scaled <- scale(for_model$age)
for_model$time_last_observed_scaled <- scale(for_model$time_to_last_obs)
for_model$haremsize_scaled <- scale(for_model$harem_size)

# Identify infinite values
infinite_values <- which(!is.finite(for_model$time_last_observed_scaled))

# Round infinite values to the nearest finite value
for_model$time_last_observed_scaled[infinite_values] <- round(for_model$time_last_observed_scaled[infinite_values])

# Check if there are any remaining infinite values
remaining_inf_values <- which(!is.finite(for_model$time_last_observed_scaled))
if (length(remaining_inf_values) > 0) {
  cat("Warning: There are still infinite values after rounding.")
}
str(for_model$time_last_observed_scaled)
for_model$time_last_observed_scaled <- as.numeric(for_model$time_last_observed_scaled)

# add control for non-converging models
control <- glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000))

# visualising raw data----
(plot <- ggplot(for_model, aes(x= with_foal_4, y = change))+geom_point()+geom_jitter())
(plot2 <- ggplot(for_model, aes(x= age, y = change))+geom_point()+geom_jitter())
(plot <- ggplot(for_model, aes(x= pregnant_3, y = change))+geom_point()+geom_jitter())
(plor  <- ggplot(for_model, aes (x = foal_alive, y = change))+ geom_point()+geom_jitter())

# null model and single variable glmms ----
# null model
null_model <- glmer (change ~ 1+ (1|year_observation)  +(1|previous_harem/name), data = for_model, family = binomial)
AIC(null_model) # 8063

# single variable models
just_time <- glmer(change ~ time_last_observed_scaled + 
                     (1|year_observation)  +(1|previous_harem/name) ,  
                   data = for_model, family = binomial, 
                   control = control)
# does not converge without control
just_age <- glmer(change ~ age_scaled + 
                    (1|year_observation)  +(1|previous_harem/name) ,  
                  data = for_model, family = binomial)
# converges without control
# aic 8181.4, r2 M: 0.0049 

just_foal <- glmer(change ~ with_foal_4 + 
                     (1|year_observation)  +(1|previous_harem/name) ,  
                   data = for_model, family = binomial)
# converges without control
summary(just_foal)
# aic 8179, r2 # 0.004

# just pregnant
just_pregnant <- glmer(change ~ pregnant_3 + 
                          (1|year_observation)  +(1|previous_harem/name) ,  
                        data = for_model, family = binomial  )
# converges without control
summary(just_pregnant)# aic   8036, r2 # 0.052

AIC(null_model, just_age, just_time, just_foal, just_pregnant)
# all models AIC < 3 units below null model.

# glmm with all effects included ----

mixed_model <- glmer(change ~ time_last_observed_scaled + age_scaled + with_foal_4 + pregnant_3+ harem_size+
                       (1|year_observation)  + (1|previous_harem/name) ,  
                     data = for_model, family = binomial  )


# AIC 7784 
# converges without control even if haremsize unscaled
r_squared <- r.squaredGLMM(mixed_model)
r_squared # 0.086 m, 0.52 c

mixed_allscaled  <- glmer(change ~ time_last_observed_scaled + age_scaled + with_foal_4 + pregnant_3+ haremsize_scaled+
                            (1|year_observation)  + (1|previous_harem/name) ,  
                          data = for_model, family = binomial,
                          control = control) # needed control here but not when harem size was unscaled !
summary(mixed_allscaled)

# AIC 7784 
# converges without control even if haremsize unscaled
r2_mixed_allscaled <- r.squaredGLMM(mixed_allscaled)
r2_mixed_allscaled  # 0.086 m, 0.52 c (same as before)


# all interactions
interaction_model <- glmer(change ~ time_last_observed_scaled + age_scaled*with_foal_4 + age_scaled*pregnant_3+ previous_haremsize +
                             (1|year_observation)  +(1|previous_harem/name) ,  
                           data = for_model, family = binomial ,
                         control = control)
# would not converge without control
summary(interaction_model) # aic 7784, interaction not significant for foal and harem size but yes for pregnant
# age scaled now significant
r_squared_i <- r.squaredGLMM(interaction_model)
r_squared_i # 0.084

# pregnant & age interaction
interaction_pregnant <- glmer (change ~ time_last_observed_scaled + age_scaled*pregnant_3 + with_foal_4+ harem_size +
                                 (1|year_observation)  +(1|previous_harem/name) ,  
                               data = for_model, family = binomial ,
                               control = control)
summary(interaction_pregnant) # aic 7781, significant interaction
r_squared_pi <- r.squaredGLMM(interaction_pregnant)
r_squared_pi # 0.085

# without harem size
nohs <- glmer (change ~ time_last_observed_scaled + age_scaled*pregnant_3 + with_foal_4+ 
                                 (1|year_observation)  +(1|previous_harem/name) ,  
                               data = for_model, family = binomial ,
                               #control = control
               )
summary(nohs) # aic 7781, significant interaction
r_squared_nohs <- r.squaredGLMM(nohs)
r_squared_nohs # 0.085

# keep everything but include foals only if they hadnt died yet

surviving_foal <- glmer(change ~ time_last_observed_scaled + age_scaled*pregnant_3 + foal_alive + harem_size +
                                (1|year_observation)  +(1|previous_harem/name) ,  
                              data = for_model, family = binomial,
                        control = control
                            )

summary(surviving_foal)
# aic 7775
r_squared_surviving <- r.squaredGLMM(surviving_foal) 
r_squared_surviving # 0.089 / 0.093 without the interaction
# effect still the same & direction too.



# Warning message:
#In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
 #              Model failed to converge with max|grad| = 0.0513923 (tol = 0.002, component 1)



# trying models with taking out one var ----
# model without age
model_noage <- glmer(change ~ time_last_observed_scaled + with_foal_4 + pregnant_3 +(1|year_observation)  + (1|previous_harem/name) ,  
                     data = for_model, family = binomial) 
# yay doesnt converge
model_nopregnant <- glmer(change ~ time_last_observed_scaled + with_foal_4 + age_scaled +(1|year_observation)  + (1|previous_harem/name) ,  
                          data = for_model, family = binomial) 
# doesnt converge
model_nofoal <- glmer(change ~ time_last_observed_scaled + pregnant_3 + age_scaled +(1|year_observation)  + (1|previous_harem/name) ,  
                          data = for_model, family = binomial) 
AIC(model_noage, model_nopregnant, model_nofoal) # all higher than the models including all of these.

# new models, with age as a 4-level factor----

ageclass_simple <- glmer(change ~ time_last_observed_scaled + age_class + with_foal_4 + pregnant_3+  harem_size+
                           (1|year_observation)  + (1|previous_harem/name) ,  
                         data = for_model, family = binomial,
                         control = control
)

summary(ageclass_simple)
# AIC 7784 - same as when age was continuous
r2_ageclasssimple <- r.squaredGLMM(ageclass_simple)
r2_ageclasssimple # 0.085 m, 0.52 c - v similar than when age was a continuous scaled variable.

# all interactions

ageclass_interactions <- glmer (change ~ time_last_observed_scaled + age_class*pregnant_3 + with_foal_4+ age_class*harem_size+
                          (1|year_observation)  +(1|previous_harem/name) ,  
                        data = for_model, family = binomial,
                        control = control
)
summary(ageclass_interactions) # aic 7781, significant interaction
r2_interactions <- r.squaredGLMM(ageclass_interactions)
r2_interactions # 0.082

# just harem
ageclass_justharem <- glmer (change ~ time_last_observed_scaled + pregnant_3 + with_foal_4+ age_class*harem_size+
                                  (1|year_observation)  +(1|previous_harem/name) ,  
                                data = for_model, family = binomial,
                                control = control
)
summary(ageclass_justharem) # aic 7781, significant interaction
r2_justharem <- r.squaredGLMM(ageclass_justharem)
r2_justharem 

# just harem
ageclass_justpregnant <- glmer (change ~ time_last_observed_scaled + pregnant_3*age_class + with_foal_4+ harem_size+
                               (1|year_observation)  +(1|previous_harem/name) ,  
                             data = for_model, family = binomial,
                             control = control
)
summary(ageclass_justpregnant) # aic 7784, significant interaction
r2_justpregnant <- r.squaredGLMM(ageclass_justpregnant)
r2_justpregnant  # 0.083


age_factor_foal_alive <- glmer (change ~ time_last_observed_scaled + age_class*pregnant_3 + foal_alive+ harem_size+
                                  (1|year_observation)  +(1|previous_harem/name) ,  
                                data = for_model, family = binomial,
                                control = control
)
summary (age_factor_foal_alive) #7778
r_sq_foalalive <- r.squaredGLMM(age_factor_foal_alive)
r_sq_foalalive




# both interactions
interaction_model_aaf <- glmer(change ~ time_last_observed_scaled + age_class*with_foal_4 + age_class*pregnant_3+
                             (1|year_observation)  +(1|previous_harem/name) ,  
                           data = for_model, family = binomial ,
                           control = control)
# would not converge without control
# takes forever to run, not sure i can wait this long :')
summary(interaction_model_aaf) # aic 7785, interaction not significant for foal but yes for pregnant - 1 of the classes
r_squared_i_aaf <- r.squaredGLMM(interaction_model_aaf)
r_squared_i_aaf # 0.084

# pregnant & age interaction
interaction_pregnant_aaf <- glmer (change ~ time_last_observed_scaled + age_class*pregnant_3 + with_foal_4+
                                 (1|year_observation)  +(1|previous_harem/name) ,  
                               data = for_model, family = binomial,
                               control = control)
summary(interaction_pregnant_aaf) # aic 7774, significant interaction
r_squared_pi_aaf <- r.squaredGLMM(interaction_pregnant_aaf)
r_squared_pi_aaf # 0.082 

# keep everything but include foals only if they hadnt died yet

model_surviving_foal_aaf <- glmer(change ~ time_last_observed_scaled + age_class*pregnant_3 + foal_alive +  
                                (1|year_observation)  +(1|previous_harem/name) ,  
                              data = for_model, family = binomial,
                              control = control
)

summary(model_surviving_foal_aaf) # aic 7777
r_squared_surviving_aaf <- r.squaredGLMM(model_surviving_foal_aaf) 
r_squared_surviving_aaf# 0.086 / 0.093 without the interaction ?
# effect still the same & direction too.

# harem size 
total_foals <- total_foals %>% mutate(haremsize_scaled =scale(previous_haremsize))
harem_model <- glmer(change ~ previous_haremsize +  (1|year_observation)  +(1|previous_harem/name) ,  
                     data = for_model, family = binomial  )
summary(harem_model)


# model w everything plus harem size 
model_everything_aaf <- glmer(change ~ time_last_observed_scaled + age_class+pregnant_3 + haremsize_scaled + with_foal_4 + 
                            (1|year_observation)  + (1|previous_harem/name) ,  
                          data = for_model, family = binomial ,
                          control = control
)
# Warning message:
#In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
#              Model failed to converge with max|grad| = 0.0513923 (tol = 0.002, component 1)

summary(model_everything_aaf) # aic 7746 and harem size effect significant
r_squared_everything_aaf <- r.squaredGLMM(model_everything_aaf) 
r_squared_everything_aaf # 0.084 :')

# 2nd age categorization----
ageclass2_simple <- glmer(change ~ time_last_observed_scaled + age_class2 + with_foal_4 + pregnant_3+  previous_haremsize+
                           (1|year_observation)  + (1|previous_harem/name) ,  
                         data = for_model, family = binomial,
                         control = control
)

summary(ageclass2_simple)
# AIC 7770 - same as when age was continuous
r2_ageclasssimple2 <- r.squaredGLMM(ageclass2_simple)
r2_ageclasssimple2 # 0.087 m, 0.52 c - v similar than when age was a continuous scaled variable.

# all interactions
ageclass2_interactions_all <- glmer (change ~ time_last_observed_scaled +
                                       age_class2*pregnant_3 + age_class2*with_foal_4+ age_class2*previous_haremsize+
                                   (1|year_observation)  +(1|previous_harem/name) ,  
                                 data = for_model, family = binomial,
                                 control = control
)
summary(ageclass2_interactions_all) # aic 7761, significant interactions  p and f, not for hs
r2_interactions_all <- r.squaredGLMM(ageclass2_interactions_all)
r2_interactions_all # 0.088
tab_model(ageclass2_interactions_all) # wrong results :(((


ageclass2_interactions_pf <- glmer (change ~ time_last_observed_scaled +
                                        age_class2*pregnant_3 + age_class2*with_foal_4+ previous_haremsize+
                                        (1|year_observation)  +(1|previous_harem/name) ,  
                                      data = for_model, family = binomial,
                                      control = control
)
summary (ageclass2_interactions_pf)
#7757
r2_pf <- r.squaredGLMM(ageclass2_interactions_pf)
r2_pf # 0.088, c 0.537

ageclass2_interactions_p <- glmer (change ~ time_last_observed_scaled +
                                        age_class2*pregnant_3 + with_foal_4+ previous_haremsize+
                                        (1|year_observation)  +(1|previous_harem/name) ,  
                                      data = for_model, family = binomial,
                                      control = control
)
summary(ageclass2_interactions_p)
# 7762 - need those 2 interactions

model_surviving_foal <- glmer(change ~ time_last_observed_scaled + age_class2*pregnant_3 + age_class2*foal_alive + previous_haremsize+ 
                                    (1|year_observation)  +(1|previous_harem/name) ,  
                                  data = for_model, family = binomial,
                                  control = control
)

summary(model_surviving_foal) # aic 7749
r_squared_surviving_aaf <- r.squaredGLMM(model_surviving_foal) 
r_squared_surviving_aaf# 0.09 / 0.093 without the interaction ?
# effect still the same & direction too.

model_sf_noi <- glmer(change ~ time_last_observed_scaled + age_class2 +pregnant_3 +foal_alive + previous_haremsize+ 
                                (1|year_observation)  +(1|previous_harem/name) ,  
                              data = for_model, family = binomial,
                              control = control
)
summary(model_sf_noi)

model_sf_alli <- glmer(change ~ time_last_observed_scaled + age_class2*pregnant_3 +age_class2*foal_alive + age_class2*previous_haremsize+ 
                         (1|year_observation)  +(1|previous_harem/name) ,  
                       data = for_model, family = binomial,
                       control = control
)
summary(model_sf_alli)
model_sf_jp <-  glmer(change ~ time_last_observed_scaled + age_class2*pregnant_3 +foal_alive + previous_haremsize+ 
                    (1|year_observation)  +(1|previous_harem/name) ,  
                  data = for_model, family = binomial,
                  control = control
)
summary(model_sf_jp)

# pregnant ----
 plogis(-2.24189)
# 0.096
 plogis(-2.24189-2.11097)
# 0.01270642
 plogis(-2.24189-0.41283)
# 0.0656
 plogis(-2.24189-2.11097-0.41283+1.02392)
# 0.02316286
 plogis(-2.24189-0.36341)
# 0.0687981
 plogis(-2.24189-2.11097-0.36341+0.88925)
# 0.02805861

# foal ----
  
   plogis(-2.24189 -1.29408)
 # 0.00283
  plogis(-2.24189-0.412838)
 # 0.065
  plogis(-2.24189-0.412838-1.29408+0.91464)
 # 0.1054457
  plogis(-2.24189-0.36461-1.29408+0.83152)  
 # 0.07706497
 plogis(  -1.51535 -0.62278  -1.31634 + 0.84925 ) 
 # 0.06880322
 

plogis(0.07)


# please dont have a lower aic
ageclass2_pf <- glmer (change ~ time_last_observed_scaled + age_class2*pregnant_3 + age_class2*with_foal_4+ harem_size+
                                   (1|year_observation)  +(1|previous_harem/name) ,  
                                 data = for_model, family = binomial,
                                 control = control
)
summary(ageclass2_pf) # 7771 thank god

# 2 interactions
ageclass2_interactions <- glmer (change ~ time_last_observed_scaled + age_class2*pregnant_3 + with_foal_4+ age_class2*harem_size+
                                  (1|year_observation)  +(1|previous_harem/name) ,  
                                data = for_model, family = binomial,
                                control = control
)
summary(ageclass2_interactions) # aic 7776, significant interaction
r2_interactions2 <- r.squaredGLMM(ageclass2_interactions)
r2_interactions2 # 0.082

# just harem
ageclass2_justharem <- glmer (change ~ time_last_observed_scaled + pregnant_3 + with_foal_4+ age_class2*harem_size+
                               (1|year_observation)  +(1|previous_harem/name) ,  
                             data = for_model, family = binomial,
                             control = control
)
summary(ageclass2_justharem) # aic 7786, significant interaction
r2_justharem2 <- r.squaredGLMM(ageclass2_justharem)
r2_justharem2 

# just pregnant
ageclass2_justpregnant <- glmer (change ~ time_last_observed_scaled + pregnant_3*age_class2 + with_foal_4+ harem_size+
                                  (1|year_observation)  +(1|previous_harem/name) ,  
                                data = for_model, family = binomial,
                                control = control
)
summary(ageclass2_justpregnant) # aic 7777, significant interaction
r2_justpregnant2 <- r.squaredGLMM(ageclass2_justpregnant)
r2_justpregnant2  # 0.083


age_factor2_foal_alive <- glmer (change ~ time_last_observed_scaled + age_class2*pregnant_3 + age_class2*foal_alive+ age_class2*harem_size+
                                  (1|year_observation)  +(1|previous_harem/name) ,  
                                data = for_model, family = binomial,
                                control = control
)
summary (age_factor2_foal_alive) #7769
r_sq_foalalive2 <- r.squaredGLMM(age_factor2_foal_alive)
r_sq_foalalive2


# plots but v low effort  ----

# Generate predicted values for the model
predictions <- ggpredict(ageclass2_interactions_all, terms = "with_foal_4 [all]")
# Plot the effects
(plot_effects <- plot(predictions))
ggsave( "przewalski/plots/age_defaultplot.png", width = 7, height = 5, dpi = 300 )


# Generate predicted values for the model
predictions <- ggpredict(mixed_model, terms = c("with_foal_4"))

# Plot the effects
(plot_effects <- plot(predictions))

effect <- Effect(c("with_foal_4"), mixed_model)

# Plot the effects
plot(effect, type = "response")

effect2 <- Effect(c("age_scaled"), mixed_model)

predictionsharem <- ggpredict(ageclass2_interactions_all2, terms = c("previous_haremsize"))

# Plot the effects
(plot_effects <- plot(predictionsharem))

# pregnant - 1 plot but with 1 line only.
fit <- as.data.frame(effect('pregnant_3', ageclass2_interactions_all))
(pregnancy_plot <- ggplot(for_model, aes(x = pregnant_3, y = change))+
   # geom_point( col = "black",
   #             position = position_dodge2(w = 0)) + # or geom point, not sure yet
    theme_classic()+
    scale_y_continuous(limits =(c(0,0.15)))+
    labs( y = ("Probability of changing groups\n"), 
          x = "Pregnant\n")+ # what is the axis label? not sure
    geom_line(aes(pregnant_3, fit, 
                  # shape = "model predictions", 
                  linetype = "model predictions"), fit,  linewidth = 1, col = "#006400") +
    scale_linetype_manual('', values =c("model predictions" = 1))+
    theme(axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10),
          axis.title = element_text(size = 12, face = "plain"),                      
          panel.grid = element_blank(), 
          legend.text=element_text(size = 10),
          plot.margin = unit(c(1,1,1,1), units = , "cm"),
          legend.title = element_blank(),
          
          legend.box.background = element_rect(colour = "darkgrey"))+
    
    scale_size_area())
ggsave("przewalski/pregnancy.png", width = 7, height = 5, dpi = 300)


foalplot <- ggplot(for_model, aes(x = with_foal_4, y = change)) +
 # geom_point(col = "black", position = position_dodge2(width = 0)) + 
  geom_line(data = fit2, aes(y = fit, linetype = "model predictions"), size = 1, col = "#006400") +
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
    legend.box.background = element_rect(colour = "darkgrey")
  ) +
  scale_linetype_manual('', values = c("model predictions" = 1)) +
  scale_size_area()

foalplot
ggsave("przewalski/foal.png", width = 7, height = 5, dpi = 300)

(age_plot <- ggplot(for_model, aes(x = age_scaled, y = change)) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), 
              se = FALSE, color = "#006400") +
  labs(y = "Probability of changing groups", x = "Age") +
  theme_classic() +
  theme(
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 12, face = "plain"),
    panel.grid = element_blank(),
    legend.text = element_text(size = 10),
    plot.margin = unit(c(1, 1, 1, 1), units = "cm"),
    legend.title = element_blank(),
    legend.box.background = element_rect(colour = "darkgrey")
  ))

pred.harem <- ggpredict(ageclass2_interactions_pf, terms = c("previous_haremsize"))  # this gives overall predictions for the model

colours = c("#BF3EFF", "#4ecb50") # new dataframe for colors

(haremplot <- ggplot(pred.harem) +
    geom_line(aes(x = x, y = predicted, shape = "model predictions", linetype = "model predictions"),
              size = 1) + 
    scale_linetype_manual('', values =c("model predictions" = 1 ))+
    scale_y_continuous (limits = c(0,0.2))+
    
 #     geom_jitter(data = for_model,                      # adding the raw data (scaled values)
 #                aes(x = previous_haremsize, y = change), size =2) +  
    # scale_colour_manual(values = colours)+
    # geom_smooth(data=for_model, method = "lm", aes(x = pregnant_3, y = change), se=FALSE, colour = "red") +
    theme_classic() +
    ylab("Probability of leaving current harem\n") +                             
    xlab("\nharem size")  +
    theme(axis.text.x = element_text(size = 15),
          axis.text.y = element_text(size = 15),
          axis.title = element_text(size = 15, face = "plain"),                      
          panel.grid = element_blank(), 
          legend.text=element_text(size = 12),
          legend.title = element_text(size = 12),
          legend.position = "bottom",
          legend.background = element_rect(colour = "grey")))

ggsave("przewalski/haremplot.png", width = 7, height = 6, dpi = 300)



# fails -----
# Load necessary libraries
library(ggplot2)
library(ggeffects)

# Generate prediction data with all combinations of predictor variables
prediction_data <- expand.grid(pregnant_3 = c(0, 1),
                               age_class2 = c("a", "b", "c"))

# Add other predictor variables to prediction data (replace mean values with actual values if available)
prediction_data$time_last_observed_scaled <- mean(for_model$time_last_observed_scaled)  # Use mean value as an example
prediction_data$with_foal_4 <- mean(for_model$with_foal_4)  # Use mean value as an example
prediction_data$harem_size <- mean(for_model$harem_size)  # Use mean value as an example

# Add random effects to prediction data
prediction_data$year_observation <- NA  # Include placeholder for year observation
prediction_data$previous_harem <- NA    # Include placeholder for previous harem
prediction_data$name <- NA              # Include placeholder for name

# Calculate predictions including random effects
pred <- ggpredict(ageclass2_interactions_all, terms = c("pregnant_3", "age_class2"), data = prediction_data)



# Create a prediction dataset with predictor variables
prediction_data <- expand.grid(age_class2 = c("a", "b", "c"), pregnant_3 = c(0, 1))

# Add other predictor variables to prediction dataset
prediction_data$time_last_observed_scaled <- mean(for_model$time_last_observed_scaled)  # Use mean value as an example
prediction_data$with_foal_4 <- mean(for_model$with_foal_4)  # Use mean value as an example
prediction_data$previous_haremsize <- mean(for_model$harem_size)  # Use mean value as an example

# Create prediction dataset
prediction_data <- expand.grid(pregnant_3 = c(0, 1), age_class2 = levels(for_model$age_class2))

# Generate predictions
pred <- ggpredict(ageclass2_interactions_all, terms = c("pregnant_3", "age_class2"))

# Generate prediction data with all combinations of predictor variables
prediction_data <- expand.grid(pregnant_3 = c(0, 1),
                               age_class2 = c("a", "b", "c"))

# Add other predictor variables to prediction data (replace mean values with actual values if available)
prediction_data$time_last_observed_scaled <- mean(for_model$time_last_observed_scaled)  # Use mean value as an example
prediction_data$with_foal_4 <- mean(for_model$with_foal_4)  # Use mean value as an example
prediction_data$previous_haremsize <- 0 # Use mean value as an example

# Calculate predictions including random effects
pred <- predict(ageclass2_interactions_all, newdata = prediction_data, type = "response", re.form = NA)

# Combine predictions with prediction data
prediction_data$predicted <- pred

# Plot the predictions
(final_plot <- ggplot(prediction_data, aes(x = pregnant_3, y = predicted, color = as.factor(age_class2))) +
  geom_line() +
  scale_color_manual(values = c("#BF3EFF", "#4ecb50", "#FF8C00")) +  # Custom colors for age classes
  theme_classic() +
  ylab("Change\n") +                             
  xlab("\nPregnant")  +
  labs(color = "age class\n") +
  theme(axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title = element_text(size = 15, face = "plain"),                      
        panel.grid = element_blank(), 
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.position = "bottom",
        legend.background = element_rect(colour = "grey")))

final_plot


# Generate prediction data with all combinations of predictor variables
prediction_data <- expand.grid(pregnant_3 = c(0, 1),
                               age_class2 = c("a", "b", "c"))

# Add other predictor variables to prediction data (replace mean values with actual values if available)
prediction_data$time_last_observed_scaled <- mean(for_model$time_last_observed_scaled)  # Use mean value as an example
prediction_data$with_foal_4 <- 0  # Use mean value as an example
prediction_data$harem_size <- mean(for_model$harem_size)  # Use mean value as an example

prediction_data$time_last_observed_scaled <- 0  # Use mean value as an example
prediction_data$with_foal_4 <- 0  # Use mean value as an example
prediction_data$previous_haremsize <- 0

# Calculate predictions including random effects
pred <- predict(ageclass2_interactions_all, newdata = prediction_data, type = "response", re.form = NA)

# Combine predictions with prediction data
prediction_data$predicted <- pred

# Plot the predictions with smooth curves
(final_plot <- ggplot(prediction_data, aes(x = pregnant_3, y = predicted, color = as.factor(age_class2))) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE) +
  scale_color_manual(values = c("#BF3EFF", "#4ecb50", "#FF8C00"),
                     labels = c("young", "prime-aged", "old"))+
  theme_classic() +
  scale_x_continuous(breaks=c(0,1), labels = c("Not pregnant", "Pregnant"))+
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
ggsave("przewalski/pregnant_plot.png", width = 7, height = 5, dpi = 300)
library(scales) # install this to get the y axis in %s


prediction_data$time_last_observed_scaled <- 0  # Use mean value as an example
 prediction_data$with_foal_4 <- 0  # Use mean value as an example
 prediction_data$previous_haremsize <- 0
 # Calculate predictions including random effects
   pred <- predict(ageclass2_interactions_pf, newdata = prediction_data, type = "response", re.form = NA)
 # Combine predictions with prediction data
   prediction_data$predicted <- pred

 (final_plot <- ggplot(prediction_data, aes(x = pregnant_3, y = predicted, color = as.factor(age_class2))) +
        geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE) +
        scale_color_manual(values = c("#BF3EFF", "#4ecb50", "#FF8C00"),
                                                 labels = c("young", "prime-aged", "old"))+
        theme_classic() +
        scale_x_continuous(breaks=c(0,1), labels = c("Not pregnant", "Pregnant"))+
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
   
   ggsave("przewalski/pregnant_plot.png", width = 7, height = 6, dpi = 300)   
   
   
   prediction_data2 <- expand.grid(with_foal_4 = c(0, 1),
                                   age_class2 = c("a", "b", "c"))
   prediction_data2$time_last_observed_scaled <- 0  
   prediction_data2$pregnant_3 <- 0  
   prediction_data2$previous_haremsize <- 0
   # Calculate predictions including random effects
   pred2 <- predict(ageclass2_interactions_pf, newdata = prediction_data2, type = "response", re.form = NA)
   # Combine predictions with prediction data
   prediction_data2$predicted <- pred2
   
   (plot_foal <- ggplot(prediction_data2, aes(x = with_foal_4, y = predicted, color = as.factor(age_class2))) +
       geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE) +
       scale_color_manual(values = c("#BF3EFF", "#4ecb50", "#FF8C00"),
                                                labels = c("young", "prime-aged", "old"))+
       theme_classic() +
       scale_x_continuous(breaks=c(0,1), labels = c("No foal", "Foal"))+
       scale_y_continuous(limits = c (0, 0.19))+
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
  ggsave("przewalski/foalplot.png", width = 7, height = 6, dpi = 300)   
  

(plot_pregnant <- ggplot(for_model, aes(x = pregnant_3, y = change, color = as.factor(age_class2))) +
    geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE) +
    scale_color_manual(values = c("#BF3EFF", "#4ecb50", "#FF8C00"),
                       labels = c("young", "prime-aged", "old"))+
    theme_classic() +
    ylab("Probability of leaving current harem\n") +                             
    xlab( "pregnant\n") +
    labs(color = "age class\n") +
    scale_y_continuous()+
    scale_x_continuous(breaks=c(0,1), labels = c("Not pregnant", "Pregnant"))+
    theme(axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 15),
          axis.title.y = element_text(size = 15, face = "plain"), 
          axis.title.x =element_blank(),
          panel.grid = element_blank(), 
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 12),
          legend.position = "bottom",
          legend.background = element_rect(colour = "grey")))


(plot_foal <- ggplot(for_model, aes(x = with_foal_4, y = change, color = as.factor(age_class2))) +
    geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE) +
    scale_color_manual(values = c("#BF3EFF", "#4ecb50", "#FF8C00"),
                       labels = c("young", "prime-aged", "old"))+
    theme_classic() +
    ylab("Probability of leaving current harem\n") +                             
    labs(color = "age class\n") +
    scale_x_continuous(breaks = c(0,1), labels = c("No foal", "Foal")) +
  #  scale_y_continuous(breaks = seq(0, 0.1, by = 0.02), labels = scales::percent_format(accuracy = 1)) +
  #      scale_x_continuous(breaks=c(0,1), labels = c("No foal", "Foal"))+
    theme(axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 15),
          axis.title.y = element_text(size = 15, face = "plain"), 
          axis.title.x =element_blank(),
          panel.grid = element_blank(), 
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 12),
          legend.position = "bottom",
          legend.background = element_rect(colour = "grey")))


(ggplot(for_model, aes(x = previous_haremsize, y = change, color = as.factor(age_class2))) +
    geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE) +
    labs(x = "Haremsize ", y = "Probability of Change") +
    theme_minimal())
# what the fuck is this

(foal_alive <- ggplot(for_model, aes(x = foal_alive, y = change, color = as.factor(age_class2))) +
    geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE) +
    scale_color_manual(values = c("#BF3EFF", "#4ecb50", "#FF8C00"),
                       labels = c("young", "prime-aged", "old"))+
    theme_classic() +
    ylab("Probability of leaving current harem\n") +                             
    xlab( "pregnant\n") +
    labs(color = "age class\n") +
    scale_y_continuous()+
    scale_x_continuous(breaks=c(0,1), labels = c("No foal", "Foal"))+
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
  
  
  prediction_data3 <- expand.grid(foal_alive = c(0, 1),
                                  age_class2 = c("a", "b", "c"))
  prediction_data3$time_last_observed_scaled <- 0  
  prediction_data3$pregnant_3 <- 0  
  prediction_data3$previous_haremsize <- 0
  # Calculate predictions including random effects
  pred3 <- predict(model_surviving_foal, newdata = prediction_data3, type = "response", re.form = NA)
  # Combine predictions with prediction data
  prediction_data3$predicted <- pred3
  
  (plot_foal_alive <- ggplot(prediction_data3, aes(x = foal_alive, y = predicted, color = as.factor(age_class2))) +
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

  # i swear these are the last changes ----
(haremsize_plot <- ggplot(haremsize, aes (x = harem_size))+
  geom_histogram())

for_model$prev_haremsize_scaled <- scale(for_model$previous_haremsize)
for_model$prev_haremsize_scaled <- as.numeric (for_model$prev_haremsize_scaled)

  scaled_simple1 <- glmer(change ~ time_last_observed_scaled + age_class2 + with_foal_4 + pregnant_3+  prev_haremsize_scaled+
                              (1|year_observation)  + (1|previous_harem/name) ,  
                            data = for_model, family = binomial,
                            control = control
  )
  
  summary(scaled_simple1)
  # AIC 7770 
 
  
  # all interactions
scaled_interactions_all1 <- glmer (change ~ time_last_observed_scaled +
                                         age_class2*pregnant_3 + age_class2*with_foal_4+ age_class2*prev_haremsize_scaled+
                                         (1|year_observation)  +(1|previous_harem/name) ,  
                                       data = for_model, family = binomial,
                                       control = control
  )
  summary(scaled_interactions_all1) # aic 7761, significant interactions  p and f, not for hs

  
  
scaled_interactions_pf <- glmer (change ~ time_last_observed_scaled +
                                        age_class2*pregnant_3 + age_class2*with_foal_4+ prev_haremsize_scaled+
                                        (1|year_observation)  +(1|previous_harem/name) ,  
                                      data = for_model, family = binomial,
                                      control = control
  )
  summary (scaled_interactions_pf)
 
  
  scaled_interactions_p <- glmer (change ~ time_last_observed_scaled +
                                       age_class2*pregnant_3 + with_foal_4+ prev_haremsize_scaled+
                                       (1|year_observation)  +(1|previous_harem/name) ,  
                                     data = for_model, family = binomial,
                                     control = control
  )
  summary(scaled_interactions_p)
  # 7762 - need those 2 interactions
  
  scaled_simple2 <- glmer(change ~ time_last_observed_scaled + age_class2*pregnant_3 + age_class2*foal_alive + prev_haremsize_scaled+ 
                                  (1|year_observation)  +(1|previous_harem/name) ,  
                                data = for_model, family = binomial,
                                control = control
  )
  
  summary(scaled_simple2) 
  
  scaled_sf_noi <- glmer(change ~ time_last_observed_scaled + age_class2 +pregnant_3 +foal_alive + prev_haremsize_scaled+ 
                          (1|year_observation)  +(1|previous_harem/name) ,  
                        data = for_model, family = binomial,
                        control = control
  )
  summary(scaled_sf_noi)
  
  scaled_sf_alli <- glmer(change ~ time_last_observed_scaled + age_class2*pregnant_3 +age_class2*foal_alive + age_class2*prev_haremsize_scaled+ 
                           (1|year_observation)  +(1|previous_harem/name) ,  
                         data = for_model, family = binomial,
                         control = control
  )
  summary(scaled_sf_alli)
  scaled_sf_jp <-  glmer(change ~ time_last_observed_scaled + age_class2*pregnant_3 +foal_alive + prev_haremsize_scaled+ 
                          (1|year_observation)  +(1|previous_harem/name) ,  
                        data = for_model, family = binomial,
                        control = control
  )
  summary(scaled_sf_jp)

  # harem as 3 level factor
  for_model<- for_model %>% 
    mutate(haremclass = case_when(
           previous_haremsize < 4 ~ "a",
          4<= previous_haremsize & previous_haremsize <= 7 ~ "b",
           previous_haremsize > 7 ~ "c"))
# favorite model but with this instead of haremsize  
  for_model$haremclass  <- as.factor(for_model$haremclass)

  haremsizefactor <- glmer(change ~ time_last_observed_scaled + age_class2*pregnant_3 + age_class2*foal_alive + haremclass+ 
                            (1|year_observation)  +(1|previous_harem/name) ,  
                          data = for_model, family = binomial,
                          control = control
  ) 
  summary(haremsizefactor)
  
  # not showing what i want so fuck this i am so done
  
  # unused plots - 11th april----
  predictions <- ggpredict(interactions_foal_pregnant, terms = "pregnant [all]")
  (plot_effects <- plot(predictions))
  
  fit <- as.data.frame(effect('pregnant', interactions_foal_pregnant))
  (pregnancy_plot <- ggplot(for_model, aes(x = pregnant, y = change))+
      # geom_point( col = "black",
      #             position = position_dodge2(w = 0)) + # or geom point, not sure yet
      theme_classic()+
      scale_y_continuous(limits =(c(0,0.15)))+
      labs( y = ("Probability of changing groups\n"), 
            x = "Pregnant\n")+ # what is the axis label? not sure
      geom_line(aes(pregnant_3, fit, 
                    # shape = "model predictions", 
                    linetype = "model predictions"), fit,  linewidth = 1, col = "#006400") +
      scale_linetype_manual('', values =c("model predictions" = 1))+
      theme(axis.text.x = element_text(size = 10),
            axis.text.y = element_text(size = 10),
            axis.title = element_text(size = 12, face = "plain"),                      
            panel.grid = element_blank(), 
            legend.text=element_text(size = 10),
            plot.margin = unit(c(1,1,1,1), units = , "cm"),
            legend.title = element_blank(),
            legend.box.background = element_rect(colour = "darkgrey"))+
      scale_size_area())
  
  # 3 lines, but cant do with predictions
  (plot_pregnant <- ggplot(for_model, aes(x = pregnant, y = change, color = as.factor(age_class))) +
      geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE) +
      scale_color_manual(values = c("#BF3EFF", "#4ecb50", "#FF8C00"),
                         labels = c("young", "prime-aged", "old"))+
      theme_classic() +
      ylab("Probability of leaving current harem\n") + 
      scale_x_continuous(breaks=c(0,1), labels = c("Not pregnant", "Pregnant"))+
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
  ggsave("przewalski/foalplot.png", width = 7, height = 6, dpi = 300) 
  

  