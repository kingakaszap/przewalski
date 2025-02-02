library (tidyverse)
library(readxl)
library (lme4)
library(MuMIn)

# assumes that the foals full dataset is already ready and imported

# current version of this variable
total_foals <- total_foals %>% 
  mutate(with_foal_3 = if_else(
    (!is.na(name_foal) & # foal born in year of obs
       dob_foal < date & # foal born before observation
       father_of_foal == previous_harem & # foal born from hst  
       (is.na(death_foal)| death_foal > date)
     |   
       (!is.na(foal_prev_year) & # foal born in year before obs
          father_of_foal_prev_year == previous_harem) &
       (is.na(death_foal_prev_year)| death_foal_prev_year > date)), 1, 0 
  ))

# i thought this would mean something but it makes it worse :(

total_foals <- total_foals %>% 
  mutate(with_foal_2 = if_else(
    (!is.na(name_foal) & # foal born in year of obs
       dob_foal < date & # foal born before observation
       father_of_foal == previous_harem) |
       (!is.na(foal_prev_year) & # foal born in year before obs
          father_of_foal_prev_year == previous_harem), 
    1, 
    0 
  ))
total_foals <- total_foals %>% 
  mutate(with_foal_4 = ifelse(
    ((!is.na(name_foal) & # foal born in year of obs
        dob_foal < date & # foal born before observation
        father_of_foal == previous_harem) | 
       (!is.na(foal_prev_year) & # foal born in year before obs
          father_of_foal_prev_year == previous_harem)), 
    1, 
    0 
  ))
# this is ok now, same model as with long ass code - or maybe not??
# THE LONGER ONE REMOVES OBSERVATIONS (PUTS AS na) WHERE WE DONT KNOW WHO FATHER OF FOAL IS.
# THE SHORTER ONE JUST PUTS THEM AS 0. 

# appearantly not because this works: 
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

total_foals <- total_foals %>% 
  mutate(with_foal_2 = if_else(
    is.na(father_of_foal) & is.na(father_of_foal_prev_year),
    NA_integer_,
    if_else(
      ((!is.na(name_foal) & # foal born in year of obs
          dob_foal < date & # foal born before observation
          father_of_foal == previous_harem) |
         (!is.na(foal_prev_year) & # foal born in year before obs
            father_of_foal_prev_year == previous_harem)), 
      1, 
      0
    )
  ))
# check later if this works
# wtf is my R2 doing though??

# pregnant
total_foals<- total_foals %>% mutate(pregnant_2 = if_else ((!is.na(name_foal)& # foal born in year of obs
                                dob_foal > date & # foal born after the observation
                                father_of_foal == harem) |# pregnant from hst  
                               (!is.na(foal_next_year) & 
                               dob_foal_next_year - date < 330 &
                                 father_of_foal == harem) # was already pregnant at time of obs
                             ,1,0 ))
# this fucked the model

# this was what originally worked:
total_foals <- total_foals %>% mutate(pregnant_2 = if_else ((!is.na(name_foal)& # foal born in year of obs
                                dob_foal > date # foal born after the observation
                                 & father_of_foal == harem) # pregnant from hst
                              |   
                               (!is.na(foal_next_year) & 
                               dob_foal_next_year - date < 330 
                              & father_of_foal == harem )# was already pregnant at time of obs
                             ,1,0 )) 

# trying model with this

for_model <-  total_foals%>% 
  filter (Gender == 2 & age > 730) %>% 
  filter(! (harem == Name_father )) %>% 
  filter (!is.na(change))

for_model$age_scaled <- scale(for_model$age)
for_model$time_last_observed_scaled <- scale(for_model$time_to_last_obs)
for_model$ foal_scaled <- scale (for_model$with_foal_2)
for_model $pregnant_scaled <- scale (for_model$pregnant_2)

mixed_model_foal_alive <- glmer(change ~ time_last_observed_scaled + age_scaled +
                       +  foal_scaled +(1|year_observation)  +(1|previous_harem/name)   ,
                     data = for_model, family = binomial,
                     control = glmerControl(optimizer = "bobyqa"))
summary(mixed_model_foal_alive)
r_squared <- r.squaredGLMM(mixed_model_foal_alive)
r_squared
AIC(mixed_model)

mixed_model_foal <- glmer(change ~ time_last_observed_scaled + age_scaled + with_foal_2 + pregnant_2+
                            (1|year_observation)  +(1|previous_harem/name) ,  
                          data = for_model, family = binomial)
summary(mixed_model_foal)
r_squared <- r.squaredGLMM(mixed_model_foal)
r_squared
AIC(mixed_model_foal) # this is the lowest i have seen so far i think. 

library(ggeffects)

# Generate predicted values for the model
predictions_age_scaled <- ggpredict(mixed_model_foal, terms = c("age_scaled"))

# Plot the effects
(plot_effects_age_scaled <- plot(predictions_age_scaled))

# Display the plot
print(plot_effects_age_scaled)

# what the fuck is wrong 

# Generate predicted values for the model
predictions <- ggpredict(mixed_model_foal, terms = c("with_foal_2"))

# Plot the effects
(plot_effects <- plot(predictions))

effect <- Effect(c("with_foal_2"), mixed_model_foal)

# Plot the effects
plot(effect, type = "response")

library(effects)
library(ggeffects)
fit <- as.data.frame(effect('with_foal_2', mixed_model_foal))
(ggplot(for_model, aes(x = with_foal_2, y = change))+
    geom_point( col = "black",
                position = position_dodge2(w = 0)) + # or geom point, not sure yet
    theme_classic()+
    labs( x = expression("\nExpansion rate (% day"^-1*")"), 
          y = "Probability of mevalonic acid presence\n")+ # what is the axis label? not sure
    geom_line(aes(with_foal_2, fit, shape = "model predictions", 
                  linetype = "model predictions"), fit,  linewidth = 1, col = "#006400") +
    scale_linetype_manual('', values =c("model predictions" = 1))+
    theme(axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10),
          axis.title = element_text(size = 12, face = "plain"),                      
          panel.grid = element_blank(), 
          legend.text=element_text(size = 10),
          # legend.box.background = element_rect(color = "grey", size = 0.3),
          plot.margin = unit(c(1,1,1,1), units = , "cm"),
          legend.title = element_blank(),
          #    legend.spacing.y = unit(0, "mm"), 
          #panel.border = element_rect(colour = "black", fill=NA),
          #   aspect.ratio = 1, axis.text = element_text(colour = 1, size = 12),
          #   legend.background = element_blank(),
          legend.box.background = element_rect(colour = "darkgrey"))+
    
    # geom_ribbon(aes( Trichome_Density, ymin = lower, ymax = upper), fit, alpha = 0.15)+
    scale_size_area())

fit_2 <- as.data.frame(effect('age_scaled', mixed_model_foal))
(ggplot(for_model, aes(x = age_scaled, y = change))+
    geom_point( col = "black",
                position = position_dodge2(w = 0)) + # or geom point, not sure yet
    theme_classic()+
    labs( x = expression("\nExpansion rate (% day"^-1*")"), 
          y = "Probability of mevalonic acid presence\n")+ # what is the axis label? not sure
    geom_line(aes(age_scaled, fit_2, shape = "model predictions", 
                  linetype = "model predictions"), fit,  linewidth = 1, col = "#006400") +
    scale_linetype_manual('', values =c("model predictions" = 1))+
    theme(axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10),
          axis.title = element_text(size = 12, face = "plain"),                      
          panel.grid = element_blank(), 
          legend.text=element_text(size = 10),
          # legend.box.background = element_rect(color = "grey", size = 0.3),
          plot.margin = unit(c(1,1,1,1), units = , "cm"),
          legend.title = element_blank(),
          #    legend.spacing.y = unit(0, "mm"), 
          #panel.border = element_rect(colour = "black", fill=NA),
          #   aspect.ratio = 1, axis.text = element_text(colour = 1, size = 12),
          #   legend.background = element_blank(),
          legend.box.background = element_rect(colour = "darkgrey"))+
    
    # geom_ribbon(aes( Trichome_Density, ymin = lower, ymax = upper), fit, alpha = 0.15)+
    scale_size_area())

# THINK ABOUT THIS : harem change probably only 1x, but lack of pregnancy / foal present in all 
# later observations.
# does this work? is it just are the ones that changed harem more likely to have 0-s,
# or are the ones that have 0-s more likely to change harems?
fixed_effects <- fixef(mixed_model)
fixed_effects_se <- sqrt(diag(vcov(mixed_model)))

# Compute Variance Inflation Factors (VIF)
vif <- fixed_effects_se^2 / (1 + fixed_effects^2)

print(vif)