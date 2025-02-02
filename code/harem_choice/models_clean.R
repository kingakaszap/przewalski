# 2024-03-11 - eternity
# new variables and maybe models

library (tidyverse)
library(readxl)
library (lme4)
library(MuMIn)

# import data----
total <-read_xlsx("przewalski/data/groups/long/full_clean.xlsx")
nagylista <- read_excel("przewalski/data/nagylista.xls", 
                        col_types = c("text", "text", "text", 
                                      "text", "date", "text", "text", "text", 
                                      "text", "text", "text", "text", "date", 
                                      "text", "date", "text", "text", "text", 
                                      "text", "text", "text", "text", "text"))

# adding new variables & joining datasets ----

# 1 - time since last observation

total$date<-as.Date(total$date)
total<- total %>% 
  group_by(name) %>% 
  # it is already ordered by date
  mutate (last_obs = as.Date(lag(date)) ) %>% 
  mutate(time_to_last_obs = as.numeric(date - last_obs)) %>% 
  ungroup()

# 2 - with foal: based on whether there was a foal born in previous year
# issue: need to add also if foal was born that year, but earlier than the observation date !!

nagylista$name<- as.character(nagylista$Name)
total_full <-dplyr::left_join(total, nagylista, by = c("name"))

total_full <- total_full %>% 
  mutate_if(is.POSIXct, as.Date) %>% # making sure dates are read correctly
  mutate(year_observation = format(as.Date(date, format = "%Y-%m-%d"), "%Y"),
         year_observation = as.numeric(year_observation),
         year_birth = format(as.Date(Date_of_birth, format = "%Y-%m-%d"), "%Y"),
         year_birth = as.numeric(year_birth),
         age = as.numeric(date- Date_of_birth)) 

 

# misc: with foal based on group composition lists----
total_full <- total_full %>% 
  rowwise() %>%
  mutate(with_foal = if_else(any(total_full$Name_mother == name &
                                   total_full$year_birth == year_observation - 1 &
                                   total_full$age <365 &
                                   total_full$Name_father ==harem| # EVERYTHING CHANGED WHEN I ADDED THIS ??
                                   total_full$Name_mother == name &
                                   total_full$Name_father ==harem&
                                   total_full$year_birth == year_observation &
                                   total_full$Date_of_birth < date), 
                             1, 0)) %>%
  mutate(with_foal = replace(with_foal, is.na(with_foal), 0)) %>%
  rowwise() %>%
  mutate(pregnant = if_else(any(total_full$Name_mother == name &
                                  total_full$year_birth == year_observation &
                                  total_full$year_birth < date&
                                  date - total_full$Date_of_birth  < 330 ), 
                            1, 0)) %>%
  mutate(pregnant = replace(pregnant, is.na(pregnant), 0) ) %>% 
  ungroup()
# data to use for models----

# 1 - removing columns i dont need
total_full <- total_full %>% 
  select(-last_obs, -N, -St.b.Num., -Name, -Place_of_birth, - Num_father, -Num_mother,
         -Number_of_Davis_case, -Branding, -Origin, -`Known_date_(YMD)`, -`Natural(N)_or_Shoot(S)`,
         -Chip, -Leavingdate, -Destination, PZPtreatment_dateofprimer, -Note, -...23)

# 2 - keeping only horses > 2 years and females, also removing when change happens from natal group
# or maybe enough just to remove those who are not in their natal group?

for_model <-  total_foals%>% 
  filter (Gender == 2 & age > 730) %>% 
  filter(! (harem == Name_father ))

# adding variable for changing harems/not
for_model <- for_model %>% 
  arrange(date) %>%
  group_by(name) %>%
  mutate(previous_harem = lag(harem)) %>%
  ungroup() %>% 
  group_by (date, name) %>% 
  mutate(change = case_when(
    harem == previous_harem ~ 0,
    harem != previous_harem ~ 1,
    is.na(previous_harem) ~ NA)) %>% 
  ungroup() %>% 
  filter (!is.na(change)) 

# trying model??----

# without random effects

for_model$age_scaled <- scale(for_model$age)
for_model$time_last_observed_scaled <- scale(for_model$time_to_last_obs)

age_model <- glm(change ~ age_scaled, data = for_model, family = binomial)
summary(age_model)

time_model <- glm(change ~ time_last_observed_scaled, data = for_model, family = binomial)
summary(time_model)

pregnancy_model <- glm(change ~ pregnant, data = for_model, family = binomial)
summary(pregnancy_model)

foal_model <- glm(change ~ with_foal_2, data = for_model, family = binomial)
summary(foal_model)

null_model <-  glm(change ~ 1, data = for_model, family = binomial)

model_combined <- glm (change ~ time_last_observed_scaled + age_scaled, data = for_model, family = binomial)
summary(model_combined)

model_everything <- glm( change ~ time_last_observed_scaled + age_scaled + with_foal + pregnant +with_foal , data = for_model, family = binomial)
summary(model_everything)

AIC(age_model, time_model,  pregnancy_model, foal_model, model_combined, model_everything,mixed_model, null_model)

for_model$year_observation <-as.factor(for_model$year_observation)

mixed_model <- glmer(change ~ time_last_observed_scaled + age_scaled +
                       +                        with_foal_2  + pregnant_2+(1|year_observation)  +(1|previous_harem/name)   , data = for_model, family = binomial)
summary(mixed_model)
# The coefficient estimate of -2.1 for the "withfoal" variable
# suggests that when "withfoal" changes from 0 to 1 (i.e., from absence to presence of foal), the log odds of "change" being 1 decreases by 2.1.
r_squared <- r.squaredGLMM(mixed_model)
r_squared
AIC(mixed_model)
# THINK ABOUT THIS : harem change probably only 1x, but lack of pregnancy / foal present in all 
# later observations.
# does this work? is it just are the ones that changed harem more likely to have 0-s,
# or are the ones that have 0-s more likely to change harems?
fixed_effects <- fixef(mixed_model)
fixed_effects_se <- sqrt(diag(vcov(mixed_model)))

# Compute Variance Inflation Factors (VIF)
vif <- fixed_effects_se^2 / (1 + fixed_effects^2)

print(vif)

# there is no way i am actually this lucky.



(plot <- ggplot(for_model, aes(x= change, y = with_foal))+geom_point()+geom_jitter())
(plot2 <- ggplot(for_model, aes(x= age, y = with_foal))+geom_point()+geom_jitter())

foal_age <- glm(with_foal ~ age, data= for_model, family = binomial)
summary(foal_age)
# gosh thats a high aic

subset_young <- total_full %>% 
  filter(Gender == "2"&
           Name_father == harem)
nrow(subset_young)



# 2024 - 03 - 13 ----
# adding foal variable differently: - RUN THIS AGAIN, I ADDED DADS NAME
foals<- select(nagylista, Name, Date_of_birth, Name_mother, Name_father, Date_of_death) 
# new: added fathers name and death
# change name mother to match the name in group list

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

write_xlsx (total_foals,"przewalski/data/total_foals.xlsx")

# adding foal born in the year before the observation

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

# this will match the year foal is born in to observations in the previous year.
# e.g. Foal is born in 2008 - will show at mother's 2007 observation. (for pregnancy)

write_xlsx (total_foals,"przewalski/data/total_foals.xlsx")

# 2024 - 03 - 14 ----
# new vars for pregnant and with foal based on nagylista data
total_foals <- total_foals %>% 
  mutate(with_foal_2 = if_else((!is.na(name_foal) & # foal born in year of obs
                                dob_foal < date & # foal born before observation
                                father_of_foal ==  harem) | # foal born from hst 
                               (!is.na(foal_prev_year) & # foal born in year before obs
                                 father_of_foal_prev_year == harem) # from harem stallion
                             ,1, 0)) %>%
  mutate(pregnant_2 = if_else ((!is.na(name_foal)& # foal born in year of obs
                                 dob_foal > date & # foal born after the observation
                                 father_of_foal == harem) |# pregnant from hst  
                               !is.na(foal_next_year) & 
                                 dob_foal_next_year - date < 330 # was already pregnant at time of obs
                                ,1,0 ))

total_issue <- total_foals %>% 
  subset(with_foal != with_foal_2 | pregnant!= pregnant_2 )

# 2024 - 03 - 16 aka i am definitely overcomplicating this now----

total_foals <- total_foals %>% 
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
#%>% 
#  filter (!is.na(change)) 

total_foals <- total_foals %>% 
  mutate(with_foal_3 = case_when(
    ((!is.na(name_foal) & # foal born in year of obs
        dob_foal < date & # foal born before observation
        father_of_foal == harem & # foal born from hst 
        change == 0 ) | 
       (!is.na(foal_prev_year) & # foal born in year before obs
          father_of_foal_prev_year == harem &
          change == 0) |
       (change == 1 &
          (!is.na(name_foal) &
             dob_foal < date &
             father_of_foal == previous_harem)) |
       (change == 1 & 
          (!is.na(foal_prev_year) &
             father_of_foal_prev_year == previous_harem))) ~ 1,
    ((is.na(name_foal) | 
        (change == 1 & !is.na(name_foal) & change == 1 & dob_foal < date & father_of_foal != previous_harem) |
        (change == 1 & is.na(foal_prev_year) & father_of_foal_prev_year != previous_harem))) ~ 0
  ))

# foal issue: only dif for ones who changed, and yet their foal is from previous HSt.  foal = 1 but sometimes 0, change = 1 in new var,
# foal = 0, change = 1 in old var.
# no issue with 0-s ! 
# remove where foals died quickly - unlikely to lead to females not leaving HSt!!!

total_foals <- total_foals %>% 
  mutate(with_foal_2 = case_when(
    ((!is.na(name_foal) & dob_foal < date & father_of_foal == harem & change == 0) | 
       (!is.na(foal_prev_year) & father_of_foal_prev_year == harem & change == 0)) ~ 1,
    (is.na(name_foal) | 
       (change == 1 & !is.na(name_foal) & dob_foal < date & father_of_foal != previous_harem) |
       (change == 1 & is.na(foal_prev_year) & father_of_foal_prev_year != previous_harem)) ~ 0
  ))
                                 
                                   
                                   
                                
 #                              ,1, 0)) %>%
 # mutate(pregnant_2 = if_else ((!is.na(name_foal)& # foal born in year of obs
                                  dob_foal > date & # foal born after the observation
                                  father_of_foal == harem) |# pregnant from hst  
                                 !is.na(foal_next_year) & 
                                 dob_foal_next_year - date < 330 # was already pregnant at time of obs
                               ,1,0 )

subset_foalissue <- subset (total_foals, with_foal_2 != with_foal_3)

write_xlsx(subset_foalissue, "przewalski/foals_issue.xlsx")

# 2024 - 03 - 17 ----
# females with number of harem changes
harem_numbers <- for_model %>% 
  group_by(name) %>% 
  summarise(harem_changes = sum(change == 1, na.rm = TRUE),
            number_of_stallions = n_distinct(harem))
unikornis <- for_model %>% 
  filter (name == "Unikornis")
ugar <- for_model %>% 
  filter (name == "Ugar")
rozmaring <- for_model %>% 
  filter (name == "Rozmaring")
epona <- for_model %>% 
  filter (name == "Epona")
emese <- for_model %>% 
  filter (name == "Emese")

lmod<- lm(number_of_stallions ~ harem_changes, data = harem_changes)
summary(lmod)
plot(lmod)
(plot <- ggplot(harem_numbers, aes(x = harem_changes, y = number_of_stallions))+
  geom_point()+
  geom_smooth(method = lm))

# 2024 - 03 - 18----
# trying to add pzp dataset 

pzp <- read_excel("przewalski/data/pzp/pzpdata.xlsx")

pzp <- pzp %>% mutate (pzp = "pzp",
                       name = Name)
View(pzp)
pzp <- pzp %>%  select(name,Primer, pzp )

total_foals <- left_join(total_foals, pzp, by = c("name"))

# 2024 - 03 - 21----
# adding pre-2008 datasets !
older_data <- read_xlsx("przewalski/data/groups/1997_2008.xlsx")
older_data <- older_data %>% 
  select (-comment) %>% 
  filter (harem != "enter" &
            harem != "escaped" &
            harem != "export" &
            harem != "uk")
total_1997<- rbind (older_data, total)
total <- total_1997

# 2021 - 03 - 24----
# harem size

haremsize<- total_foals %>% 
  filter   (!age < 730,
               harem != Name_father,
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

View(haremsize)

haremsize <- haremsize %>% 
  group_by(harem) %>% 
  mutate(harem_start = as.Date(min(date)),
         harem_experience = as.numeric(date - harem_start))

filtered<- haremsize %>% 
  filter(!grepl('UK', harem)) %>% 
  filter(!grepl('unk', harem)) %>% 
  filter(!grepl('uk', harem))
View(filtered)
# 2024 - 03 - 25----

# this doesnt make my R2 go back to the way it was :(

data_smaller <- total_foals %>%  
  subset (!
    ( !is.na(dob_foal)& dob_foal < date & father_of_foal != previous_harem & is.na(foal_prev_year) )  |
     (  !is.na(dob_foal) & father_of_foal != previous_harem & !is.na(foal_prev_year) & father_of_foal_prev_year != previous_harem) |
      (is.na(dob_foal)& !is.na(foal_prev_year) & father_of_foal_prev_year != previous_harem) |
     ( !is.na(dob_foal) & dob_foal > date & father_of_foal != previous_harem & is.na(foal_next_year)) |
      (  !is.na(dob_foal) & dob_foal > date & father_of_foal != previous_harem & !is.na(foal_next_year) & father_of_foal_next_year != previous_harem) |
      !is.na(dob_foal_next_year) & father_of_foal_next_year != previous_harem & (is.na(dob_foal ) | dob_foal < date)
  )
for_model_smaller <-  data_smaller%>% 
  filter (Gender == 2 & age > 730) %>% 
  # only females older than 2
  filter(! (previous_harem == Name_father & age < 1095)) %>% 
  # post-dispersal females only - change to previous harem? doesnt fuck up main effects. Add reference to age though: older than 3 years OK
  filter (date > as.Date("2008-01-01")) %>%
  # observations from 2008 onwards
  filter (!is.na(change),
          !is.na(age),
          !is.na(with_foal_4),
          !is.na(pregnant_3))
# only keep data where there is data for all variables for models to be comparable

# scale variables for convergence
for_model_smaller$age_scaled <- scale(for_model_smaller$age)
for_model_smaller$time_last_observed_scaled <- scale(for_model_smaller$time_to_last_obs)    
# same model on this data
mixed_model_smaller <- glmer(change ~ time_last_observed_scaled + age_scaled + with_foal_4 + pregnant_3+ 
                       (1|year_observation)  + (1|previous_harem/name) ,  
                     data = for_model_smaller, family = binomial  )
summary(mixed_model_smaller) # AIC 7736.
r_squared_smaller <- r.squaredGLMM(mixed_model_smaller)
r_squared_smaller # 0.089 m, 0.56 c

# frequency distribution of harem sizes

(fdhs <- ggplot(for_model, aes (x = harem_size))+
    geom_histogram(bins = 18)+
    theme_classic())

# 2024 - 03 - 26----
# trying to convert age into categorical
# 1 - females younger than 5
# 2 - females 5-10
# females older than 10
10*365

for_model <- for_model %>% 
  mutate(age_class = 
           case_when (age < 1095 ~ "2years" ,
                       (age  >= 1095 & age <1825) ~ "primiparous",
                      age >= 1825 & age <= 3650 ~ "prime",
                      age > 3650 ~ "old"))
age_as_factor <- glmer (change ~ time_last_observed_scaled + age_class+ pregnant_3 + with_foal_4+ harem_size+
                                 (1|year_observation)  +(1|previous_harem/name) ,  
                               data = for_model, family = binomial,
                        control = control
                        )
summary(age_as_factor) # aic 7781, significant interaction
r_squared_aaf <- r.squaredGLMM(age_as_factor)
r_squared_aaf # 0.087
chi_sq <- summary(age_as_factor)$null.deviance
chi_sq
age_factor_foal_alive <- glmer (change ~ time_last_observed_scaled + age_class+ pregnant_3 + foal_alive+ harem_size+
                          (1|year_observation)  +(1|previous_harem/name) ,  
                        data = for_model, family = binomial,
                       control = control
)
summary (age_factor_foal_alive)
r_sq_foalalive <- r.squaredGLMM(age_factor_foal_alive)
r_sq_foalalive
# 2024 - 03 - 28----
# age categories: seeing frequency distribution and visual stuff

(ageplot <- ggplot (for_model, aes (x = age_years, y = pregnant_3))+
   scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22))+
   geom_point()+
   geom_jitter()+
   theme_classic())

for_model<- for_model %>% 
  mutate(age_years = age/365)

(ageplot_fd <- ggplot(for_model, aes( x = age_years))+
    geom_histogram(binwidth = bw)+
    theme_classic())
bw <- 2 * IQR(for_model$age_years) / length(for_model$age_years)^(1/3)

# 2024 - 03 - 29----
# playing around with age even more.
subset2 <- for_model
# age as factor
subset2 <- subset2 %>% 
  mutate(age_class = 
           case_when (age < 1460 ~ "a" ,
                      age  >= 1460 & age <2190 ~ "b",
                      age >= 2190 & age < 3650 ~ "c" ,
                      age >= 3650 ~ "d") )

mod1 <- glmer (change ~ time_last_observed_scaled + age_class*pregnant_3 + age_class*with_foal_4+ haremsize_scaled+
                          (1|year_observation)  +(1|previous_harem/name) ,  
                        data = subset2, family = binomial,
                        control = control
)
summary(mod1) # aic 7781, significant interaction
r_squared_aaf <- r.squaredGLMM(age_as_factor)
r_squared_aaf # 0.087

age_factor_foal_alive <- glmer (change ~ time_last_observed_scaled + age_class+ pregnant_3 + foal_alive+ harem_size+
                                  (1|year_observation)  +(1|previous_harem/name) ,  
                                data = for_model, family = binomial,
                                control = control
)
summary (age_factor_foal_alive)
r_sq_foalalive <- r.squaredGLMM(age_factor_foal_alive)
r_sq_foalalive

mod2 <- glmer(change ~ time_last_observed_scaled + age_class + with_foal_4 + pregnant_3+  haremsize_scaled+
                           (1|year_observation)  + (1|previous_harem/name) ,  
                         data = subset2, family = binomial,
                         control = control)

summary(mod2)

(plot_ageclass <- ggplot(subset2, aes (x = age_class))+ stat_count()+ geom_bar())

# AIC 7749
r_squared_aaf1 <- r.squaredGLMM(mixed_model_aaf)
r_squared_aaf1 # 0.085 m, 0.51 c - exactly the same as age as a continuous scaled variable.


# both interactions
mod3 <- glmer(change ~ time_last_observed_scaled + age_class*with_foal_4 + age_class*pregnant_3+
                                 (1|year_observation)  +(1|previous_harem/name) ,  
                               data = for_model, family = binomial ,
                               control = control)
# would not converge without control
# takes forever to run, not sure i can wait this long :')
summary(interaction_model_aaf) # aic 7785, interaction not significant for foal but yes for pregnant - 1 of the classes
r_squared_i_aaf <- r.squaredGLMM(interaction_model_aaf)
r_squared_i_aaf # 0.084

# pregnant & age interaction
mod4 <- glmer (change ~ time_last_observed_scaled + age_class*pregnant_3 + with_foal_4+
                                     (1|year_observation)  +(1|previous_harem/name) ,  
                                   data = subset2, family = binomial,
                                   control = control)
summary(mod4) # aic 7774, significant interaction
r_squared_pi_aaf <- r.squaredGLMM(interaction_pregnant_aaf)
r_squared_pi_aaf # 0.082 

# keep everything but include foals only if they hadnt died yet

model_surviving_foal_aaf <- glmer(change ~ time_last_observed_scaled + age_class*pregnant_3 + foal_alive +  
                                    (1|year_observation)  +(1|previous_harem/name) ,  
                                  data = for_model, family = binomial,
                                  control = control
)

summary(model_surviving_foal_aaf) # aic 7769
r_squared_surviving_aaf <- r.squaredGLMM(model_surviving_foal_aaf) 
r_squared_surviving_aaf# 0.086 / 0.093 without the interaction ?
# effect still the same & direction too.

# harem size 
total_foals <- total_foals %>% mutate(haremsize_scaled =scale(previous_haremsize))
harem_model <- glmer(change ~ previous_haremsize +  (1|year_observation)  +(1|previous_harem/name) ,  
                     data = for_model, family = binomial  )
summary(harem_model)


# model w everything plus harem size 
mod5 <- glmer(change ~ time_last_observed_scaled + age_class*pregnant_3 + haremsize_scaled + with_foal_4 + 
                                (1|year_observation)  + (1|previous_harem/name) ,  
                              data = for_model, family = binomial ,
                              control = control
)
# Warning message:
#In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
#              Model failed to converge with max|grad| = 0.0513923 (tol = 0.002, component 1)

summary(mod5) # aic 7746 and harem size effect significant
r_squared_everything_aaf <- r.squaredGLMM(mod5) 
r_squared_everything_aaf # 0.084 :')
# plots
# Check if the number of unique values in haremsize_scaled is less than num_haremsize_scaled
if (length(unique(for_model$haremsize_scaled)) < num_haremsize_scaled) {
  num_haremsize_scaled <- length(unique(for_model$haremsize_scaled))
}

# Sample a subset of unique values for haremsize_scaled
sampled_haremsize_scaled <- sample(unique(for_model$haremsize_scaled), num_haremsize_scaled)

# Create a grid of predictor values for plotting
newdata <- expand.grid(haremsize_scaled = sampled_haremsize_scaled,
                       age_class = levels(subsampled_data$age_class),
                       pregnant_3 = mean(subsampled_data$pregnant_3),  # Using the mean for binary variable
                       with_foal_4 = mean(subsampled_data$with_foal_4),  # Using the mean for binary variable
                       time_last_observed_scaled = mean_time_last_observed_scaled,  # Mean value for time_last_observed_scaled
                       year_observation = unique(subsampled_data$year_observation),  # Include unique values of year_observation
                       previous_harem = sampled_combinations$previous_harem,
                       name = sampled_combinations$name)  # Include sampled combinations of previous_harem and name

# Make predictions for each combination of fixed and random effects
predictions <- predict(model_everything_aaf, type = "response", newdata = newdata)

# Transform predictions into probabilities
probabilities <- plogis(predictions)

# Combine predictions with newdata
newdata$Probability_of_Change <- probabilities

# Plot the probabilities
library(ggplot2)
ggplot(newdata, aes(x = haremsize_scaled, y = Probability_of_Change, color = age_class)) +
  geom_line() +
  labs(x = "Haremsize Scaled", y = "Probability of Change") +
  theme_minimal()


library(ggplot2)

# Plot the probabilities using geom_smooth
(ggplot(for_model, aes(x = harem_size, y = change, color = (as.factor(age_class2)))) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE) +
  labs(x = "Haremsize ", y = "Probability of Change") +
  theme_minimal())

(ggplot(for_model, aes(x = pregnant_3, y = change, color = (as.factor(age_class2)))) +
    geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE) +
    labs(x = "pregnant ", y = "Probability of Change") +
    theme_minimal())

(ggplot(for_model, aes(x = with_foal_4, y = change, color = (as.factor(age_class2)))) +
    geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE) +
    labs(x = "foal ", y = "Probability of Change") +
    theme_minimal())

subset_ageclass <- for_model %>% 
  subset (age_class2 == "c")

(plot_hs <- ggplot(subset_ageclass, aes (x = harem_size))+
    geom_histogram()+
    theme_bw())

# Generate predicted probabilities for each combination of predictor values
predictions <- predict(ageclass2_interactions_all, type = "response")

# Combine the predicted probabilities with the original data
for_model$predicted_prob <- predictions

# Generate model predictions
predictions <- predict(ageclass2_interactions_all, type = "response")

# Combine the predicted probabilities with the original data
for_model$predicted_prob <- predictions

# Plotting the relationship between "pregnant_3" and "change" stratified by age class
(ggplot(for_model, aes(x = pregnant_3, y = predicted_prob, color = as.factor(age_class2))) +
  geom_line(aes(y = predicted_prob), size = 1) +  # Add lines for the model predictions
  labs(x = "Pregnant", y = "Predicted Probability of Change"))
(ggplot(prediction_data, aes(x = pregnant_3, y = predictions, color = as.factor(age_class2))) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE) +
  labs(x = "Pregnant", y = "Probability of Change") +
  theme_minimal())

# Define reference levels for predictor variables
reference_levels <- data.frame(time_last_observed_scaled = mean(for_model$time_last_observed_scaled),
                               age_class2 = rep(c("a", "b", "c"), each = 2),
                               pregnant_3 = rep(0:1, times = 3),
                               with_foal_4 = mean(for_model$with_foal_4),
                               harem_size = mean(for_model$harem_size))

# Calculate intercepts based on model estimates
intercepts <- predict(ageclass2_interactions_all, newdata = reference_levels, type = "link", re.form = NA)

# Convert intercepts to probabilities
intercepts_prob <- plogis(intercepts)

# Plot predictions
(ggplot(prediction_data, aes(x = pregnant_3, y = predictions, color = as.factor(age_class2))) +
 # geom_point() +  # Add points for predicted values
  geom_line(data = reference_levels, aes(y = intercepts_prob, group = age_class2), linetype = "dashed") +
  labs(x = "Pregnant", y = "Probability of Change") +
  theme_minimal())


# looks ok they do occur in small harems
# 2024 - 03 - 31----
# age class, same model, with setting reference level to middle aged.
for_model <- for_model %>% 
  mutate(age_class3 = 
           case_when (age <= 1460 ~ "b" , # younger than 4
                      age  > 1460 & age <= 3650 ~ "a", # between 4 and 10
                      age > 3650 ~ "c") ) # older than 10
for_model$age_class3 <- as.factor(for_model$age_class3)
ageclass3_interactions_all <- glmer (change ~ time_last_observed_scaled + age_class3*pregnant_3 + age_class3*with_foal_4+ age_class3*harem_size+
                                       (1|year_observation)  +(1|previous_harem/name) ,  
                                     data = for_model, family = binomial,
                                     control = control
)
summary(ageclass3_interactions_all) # aic 7770, significant interactions

r2_interactions_all <- r.squaredGLMM(ageclass2_interactions_all)
r2_interactions_all # 0.088
tab_model(ageclass2_interactions_all)