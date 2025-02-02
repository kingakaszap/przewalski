library (tidyverse)
library(readxl)
library (lme4)
library(MuMIn)
library(effects)
library(ggeffects)
library(writexl)

# import data ----
total <-read_xlsx("przewalski/data/groups/long/full_clean.xlsx")
nagylista <- read_excel("przewalski/data/nagylista.xls", 
                        col_types = c("text", "text", "text", 
                                      "text", "date", "text", "text", "text", 
                                      "text", "text", "text", "text", "date", 
                                      "text", "date", "text", "text", "text", 
                                      "text", "text", "text", "text", "text"))

# joining datasets, adding new variables ----

# 1 - time since last observation
total$date<-as.Date(total$date)
total<- total %>% 
  group_by(name) %>% 
  # it is already ordered by date
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
  ungroup() %>% 
  filter (!is.na(change)) # removing observations where there is no data on previous harem
# this might need to move further down if i do anything with harem size.
# maybe move to when I start to do models!

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
# creating with foal and pregnant vars----
total_foals <- total_foals %>% 
  mutate(with_foal_2 = case_when(
    ((!is.na(name_foal) & # foal born in year of obs
        dob_foal <= date & # foal born before observation
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
        (change == 1 & !is.na(name_foal) & dob_foal < date & father_of_foal != previous_harem) |
        (change == 1 & is.na(foal_prev_year) & father_of_foal_prev_year != previous_harem))) ~ 0
  ))
# this should work without including change :(((

total_foals <- total_foals %>% 
  mutate(with_foal_4 = case_when(
    (!is.na(name_foal) & # foal born in year of obs
       dob_foal <= date & # foal born before observation
       father_of_foal == previous_harem) | 
      (!is.na(foal_prev_year) & # foal born in year before obs
         father_of_foal_prev_year == previous_harem) ~ 1,
    TRUE ~ 0
  ))


# try changing to previous harem
total_foals <- total_foals %>% 
  mutate(pregnant_2 = case_when(
    ((!is.na(name_foal) & # foal born in year of obs
        dob_foal > date & # foal born after observation
        father_of_foal == harem & # foal born from hst 
        change == 0 ) | 
       (!is.na(foal_next_year) & # foal born in year before obs
          dob_foal_next_year - date < 330 &
          father_of_foal_next_year == harem &
          change == 0) |
       (change == 1 &
          (!is.na(name_foal) &
             dob_foal > date &
             father_of_foal == previous_harem)) |
       (change == 1 & 
          (!is.na(foal_next_year) &
             dob_foal_next_year - date < 330 &
             father_of_foal_next_year == previous_harem))) ~ 1,
    ((is.na(name_foal)  | 
        (change == 1 & !is.na(name_foal) & dob_foal > date & father_of_foal != previous_harem) |
        (change == 1 & is.na(foal_next_year) & dob_foal_next_year - date < 330 & father_of_foal_next_year != previous_harem))) ~ 0
  ))
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

foalissue <- subset (total_foals, pregnant_3 != pregnant_2)

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


# models----
for_model <-  total_foals%>% 
  filter (Gender == 2 & age > 730) %>% 
  filter(! (harem == Name_father )) %>% 
  filter (date > as.Date("2008-01-01")) %>% 
  filter (!is.na(change),
          !is.na(age),
          !is.na(with_foal_4),
          !is.na(pregnant_3))

for_model$age_scaled <- scale(for_model$age)
for_model$time_last_observed_scaled <- scale(for_model$time_to_last_obs)
control <- glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000))

mixed_model <- glmer(change ~ time_last_observed_scaled + age_scaled + with_foal_4 + pregnant_3+ 
                            (1|year_observation)  +(1|previous_harem/name) ,  
                          data = for_model, family = binomial  
                     #control = control
                     )
summary(mixed_model)
r_squared <- r.squaredGLMM(mixed_model)
r_squared # 0.089 m, 0.56 c

null_model <- glmer (change ~ 1+ (1|year_observation)  +(1|previous_harem/name), data = for_model, family = binomial)
AIC(null_model) # 8190.77 perf

AIC(mixed_model) # 7893
# plogis()
plogis(-0.56)
# pzp 
total_foals <- total_foals %>% 
  mutate(pzp_treated = if_else(is.na(pzp), 0, if_else(pzp == "pzp" & Primer < date, 1, 0)))

pzp_model <- glmer(change ~ time_last_observed_scaled + age_scaled + with_foal_4 + pregnant_3+ pzp_treated +
                       (1|year_observation)  +(1|previous_harem/name) ,  
                     data = for_model, family = binomial
                     #control = control
)
summary(pzp_model)
r_squared <- r.squaredGLMM(pzp_model)
r_squared


 # trying simpler models, comparing AIC-s. ----
just_time <- glmer(change ~ time_last_observed_scaled + 
                       (1|year_observation)  +(1|previous_harem/name) ,  
                     data = for_model, family = binomial  
                     #control = control
)
# does not converge
just_age <- glmer(change ~ age_scaled + 
                    (1|year_observation)  +(1|previous_harem/name) ,  
                  data = for_model, family = binomial  
                  #control = control
)
summary(just_age)
r_squared <- r.squaredGLMM(just_age)
r_squared
# aic 8171.9,, r2 M: 0.0049 

# just foal

just_foal <- glmer(change ~ with_foal_4 + 
                    (1|year_observation)  +(1|previous_harem/name) ,  
                  data = for_model, family = binomial  
                  #control = control
)
summary(just_foal)
# aic 8169.8 
r_squared <- r.squaredGLMM(just_foal)
r_squared # 0.004


# just pregnant
just_pregnantl <- glmer(change ~ pregnant_3 + 
                     (1|year_observation)  +(1|previous_harem/name) ,  
                   data = for_model, family = binomial  
                   #control = control
)
summary(just_pregnantl)
# aic   8030.9 
r_squared <- r.squaredGLMM(just_pregnantl)
r_squared # 0.052

interaction_model <- glmer(change ~ time_last_observed_scaled + age_scaled*with_foal_4 + pregnant_3+
                       (1|year_observation)  +(1|previous_harem/name) ,  
                     data = for_model, family = binomial ,
                     control = control
)
# would not converge without control
summary(interaction_model) # aic 7867.6, interaction not significant
r_squared <- r.squaredGLMM(interaction_model)
r_squared # 0.091

interaction_pregnant <- glmer (change ~ time_last_observed_scaled + age_scaled*pregnant_3 + with_foal_4+
                                       (1|year_observation)  +(1|previous_harem/name) ,  
                                     data = for_model, family = binomial ,
                                     control = control
)
summary(interaction_pregnant) # aic 7862.7, significant interaction
r_squared <- r.squaredGLMM(interaction_pregnant)
r_squared # 0.90


# keep everything but include foals only if they hadnt died yet

model_surviving_foal <- glmer(change ~ time_last_observed_scaled + age_scaled + foal_alive + pregnant_3+ 
                       (1|year_observation)  +(1|previous_harem/name) ,  
                     data = for_model, family = binomial , 
                     control = control
)
# does not converge without control
summary(model_surviving_foal)
r_squared <- r.squaredGLMM(model_surviving_foal)
r_squared # 0.096, 0.567 aic = 7858
# lower aic, but had to kind of force it to converge? ffs

# plots but v low effort  ----
library(ggeffects)

# Generate predicted values for the model
predictions_age_scaled <- ggpredict(mixed_model, terms = "age_scaled [all]")
# Plot the effects
(plot_effects_age_scaled <- plot(predictions_age_scaled))
ggsave( "przewalski/plots/age_defaultplot.png", width = 7, height = 5, dpi = 300 )


# Generate predicted values for the model
predictions <- ggpredict(mixed_model, terms = c("with_foal_4"))

# Plot the effects
(plot_effects <- plot(predictions))

effect <- Effect(c("with_foal_4"), mixed_model)

# Plot the effects
plot(effect, type = "response")
ggsave("przewalski/plots/foal_default_1.png", width = 7, height = 5, dpi = 300)


fit <- as.data.frame(effect('pregnant_3', mixed_model))
(ggplot(for_model, aes(x = pregnant_3, y = change))+
    geom_point( col = "black",
                position = position_dodge2(w = 0)) + # or geom point, not sure yet
    theme_classic()+
    labs( y = expression("\nProbability of changing groups"), 
          x = "Pregnant\n")+ # what is the axis label? not sure
    geom_line(aes(with_foal_2, fit, shape = "model predictions", 
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

ggsave  ("przewalski/uni/foal_default_2.png", width = 7, height = 5, dpi = 300)

# run this code 1st thing tomorrow, see if it works. Then try vif. Then try minor alterations