
library (tidyverse)
library(readxl)
library (lme4)


total <-read_xlsx("przewalski/data/groups/long/full_clean.xlsx")

# 1) add column for time since the last observation

# issue: later years - only month given, dates arbitarily given
total$date<-as.Date(total$date)
str(total)
total<- total %>% 
  group_by(name) %>% 
  # issue: lots of horses wont have a previous observation because its the first time
  # they have been observed, or especially with UK-s :(
  # but - whether they changed harem or not can also not be determined for these horses!
  # so maybe ok?
mutate (last_obs = as.Date(lag(date)) ) %>% 
        # %>%  # unsure if this does the thing lol
  mutate(time_to_last_obs = as.numeric(date - last_obs)) %>% 
  ungroup()
# I AM NOT SURE THE GROUPING WORKS HERE!!

# thinking about a model:

# try: first with 2 effects: time from last observation, 
# and age (should be foal in the previous year but that takes a bit more thinking.. boo)

# 1 - horses to exclude
# not doing harem size here, so can just exclude whoever. 

# 1) exclude horses that are too young
# < 3 years
# only females: gender
# have the age data

total <- total %>% 
  select(-last_obs)


total_full <-dplyr::left_join(total, nagylista, by = c("name"))

total_full <- total_full %>% 
  select(-Note, -Chip, - Name)  %>% 
  mutate_if(is.POSIXct, as.Date) %>% 
  mutate(year_observation = format(as.Date(date, format = "%Y-%m-%d"), "%Y"),
         year_observation = as.numeric(year_observation),
         year_birth = format(as.Date(Date_of_birth, format = "%Y-%m-%d"), "%Y"),
         year_birth = as.numeric(year_birth)) 

total_full<- total_full %>% 
  rowwise() %>%
  mutate(with_foal = if_else(any(total_full$Name_mother == name &
                                   total_full$year_birth == year_observation - 1), 1, 0)) %>%
  mutate(with_foal = replace(with_foal, is.na(with_foal), 0)) %>% 
  ungroup()

# would be better to search through both total_full and just nagylista - can I do that?
  
total_full<- total_full %>% 
    rowwise() %>%
    mutate(pregnant = if_else(any(total_full$Name_mother == name &
                                     total_full$year_birth == year_observation  ), 1, 0)) %>%
    mutate(pregnant = replace(pregnant, is.na(pregnant), 0) ) %>% 
  ungroup() 
# THINK THROUGH IF THIS ACTUALLY CONVEYS HORSE BEING PREGNANT! 11month pregnancy, births in spring usually 

total_full <- total_full %>% 
  mutate(age = as.numeric(date - Date_of_birth)) %>% 
  ungroup()

total_full$with_foal<- as.factor(total_full$with_foal)
str(total_full$with_foal)
for_model <- total_full %>% 
  filter (Gender == 2 & age > 730)

for_model <- for_model %>% 
  arrange(date) %>%
  group_by(name) %>%
  mutate(previous_harem = lag(harem)) %>%
  ungroup()

for_model <- for_model %>% 
  group_by (date, name) %>% 
  mutate(change = case_when(
    harem == previous_harem ~ 0,
    harem != previous_harem ~ 1,
    is.na(previous_harem) ~ NA)) %>% 
  ungroup()

for_model<- for_model %>% 
  filter (!is.na(change)) %>% 
  filter(! (harem == Name_father ))

first_model <- glm(change ~ age, data = for_model, family = binomial)
summary(first_model)
first_model

second_model <- glm(change ~ time_to_last_obs, data = for_model, family = binomial)
summary(second_model)

model_combined <- glm (change ~ time_to_last_obs + age, data = for_model, family = binomial)
model_combined2 <- glm( change ~ time_to_last_obs + age + with_foal +pregnant  , data = for_model, family = binomial)
summary(model_combined2)
model_pregnancy <- glm(change ~ pregnant, data = for_model, family = binomial)
summary(model_pregnancy)
model_foal <- glm(change ~ with_foal, data = for_model, family = binomial)
summary(model_foal)
null_model <-  glm(change ~ 1, data = for_model, family = binomial)
AIC(first_model, second_model, null_model, model_combined, model_pregnancy, model_foal, model_combined2)

for_model$year_observation <-as.factor(for_model$year_observation)
for_model$with_foal <- as.factor(for_model$with_foal)
for_model$pregnant <- as.factor(for_model$pregnant)
for_model$age_scaled <- scale(for_model$age)
for_model$time_last_observed_scaled <- scale(for_model$time_to_last_obs)
mixed_model <- glmer(change ~ time_last_observed_scaled + age_scaled + with_foal
                       pregnant +(1|year_observation) + (1|harem/name)   , data = for_model, family = binomial)
# harem here is harem no #2 !!!
summary(mixed_model)
AIC(mixed_model)
#R2 glmm (model) - use code!
# explanatory variables:

# 1 - with foal: pregnant in previous year
for_model <- for_model %>% 
  mutate(year_observation = format(as.Date(date, format = "%Y-%m-%d"), "%Y"),
         year_observation = as.numeric(year_observation),
         year_birth = format(as.Date(Date_of_birth, format = "%Y-%m-%d"), "%Y"),
         year_birth = as.numeric(year_birth),
         with_foal = case_when(year_birth == year_observation - 1 & Name_mother == name ~ 1,
                                TRUE ~ 0))

# trying in nagylista
nagylista<- nagylista %>% 
  mutate(
  year_birth = format(as.numeric(as.Date(Date_of_birth, format = "%Y-%m-%d"), "%Y"))
  nagylista<- nagylista %>% 
    
# WRONG - since youngsters already not present in this dataset!! be a bit smarter pls
# 2 - pregnant - foal in next year
    
    
# unused shit code ----
    #  mutate(
    #       with_foal = case_when(
    #        year_birth == year_observation - 1  ~1,
    # & Name_mother == name ~ 1, THIS IS WHERE THE ISSUE IS BUT WHY
    #       TRUE ~ 0)) %>% 
  #group_by(name) %>%
  # mutate(with_foal = ifelse((year_observation - 1) %in% lag(year_birth) & 
  #    lag(name) == Name_mother, 1, 0)) %>%
