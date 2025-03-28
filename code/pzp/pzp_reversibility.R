# libraries and data import----
library(tidyverse)
library(readxl)
library(ggrepel)
library(lubridate)
library(gridExtra)

pzp <- read_excel("data/pzpdata.xlsx")
pzp <- mutate_if(pzp, is.POSIXct, as.Date)
pzp$Foal_DOB_P_9yr <- as.Date(pzp$Foal_DOB_P_9yr)
pzp$Foal_DOB_P_10yr <- as.Date(pzp$Foal_DOB_P_9yr)

# CURRENTLY WITH 2 YRS discarding treatment, just efficacy -> birth again -----
# got an initial primer and booster
pandb <- pzp %>%  filter(!is.na(Primer)& !is.na(Booster1))

str(pandb [, 30:39])

# had at least 3 years of recorded infertility
efficient <- pandb %>%
  filter(apply(select(., 30:39), 1, function(row) {
      runs <- rle(row == "NP")
      any(runs$values & runs$lengths >= 2) }))
# maybe change to 2 years?
  
View(efficient)
nrow(efficient) # 62/85

# make a variable for whether they gave birth post infertility period
# attempt 1 - 
# if 3 years infertile and then birth again, 1,
# if not than 0
efficient <- efficient %>%
  mutate(birth_again = apply(select(., 30:39), 1, function(row) {
    # Get run lengths 
    runs <- rle(row == "NP")
    # check for at least one run of 3 or more NAs followed by a non-NA
    for (i in seq_along(runs$lengths)) {
      if (runs$values[i] == TRUE && runs$lengths[i] >= 3) { 
        # At least 3 consecutive "NP"
        # Calculate the starting index for the remaining values after the NP run
        start_idx <- sum(runs$lengths[1:i]) + 1
        remaining_values <- row[start_idx:length(row)]
        # Check if there's any "YES" in the remaining values, ignoring NA values
        if (any(remaining_values == "YES", na.rm = TRUE)) {
          return(1) }}}
    return(0) }))

# attempt 2 - separate non-reversible and data deficient
efficient <- efficient %>%
  mutate(birth_again = apply(select(., 30:39), 1, function(row) {
    runs <- rle(row == "NP")
    # initialise
    first_condition_met <- FALSE
    # Check if: 3 or more consecutive "NP"s followed by a "YES"
    for (i in seq_along(runs$lengths)) {
      # runs already made in prev code - if delete that then need to put it here
      if (runs$values[i] == TRUE && runs$lengths[i] >= 2) { 
        # At least 3 consecutive "NP"
        start_idx <- sum(runs$lengths[1:i]) + 1
        remaining_values <- row[start_idx:length(row)]
        if (any(remaining_values == "YES", na.rm = TRUE)) {
          first_condition_met <- TRUE
          break}}}
    if (first_condition_met) {
      return(1) }
    # Check for the second condition: 5 or more consecutive "NP"s
    if (any(runs$values & runs$lengths >= 5)) {
      return(0)}
    return(2)  }))

efficient_zero <- efficient %>% filter(birth_again == 0)
efficient_one <- efficient %>% filter (birth_again == 1)
efficient_excluded <- efficient %>%  filter (birth_again == 2)
efficient_unambigous <- efficient %>% filter((birth_again ==1) |(birth_again ==0) )
efficient_unambigous$age_at_primer <- as.numeric(efficient_unambigous$Primer - efficient_unambigous$birth_date)
efficient_unambigous$vaccines_number <- rowSums(!is.na(efficient_unambigous[, c(5:12)]))


# is there an effect on the above grouping of a) age at primer and b) number of doses received?
model_age <- glm(birth_again ~ age_at_primer, data = efficient_unambigous, family = binomial )
summary(model_age)
# no effect, but also very small sample size
model_vaccines <- glm(birth_again ~ vaccines_number, data = efficient_unambigous, family = binomial )
summary(model_vaccines)
# :') the more vaccines received the more likely they are to be in the group that came back to birth
# small sample size, but also: maybe they were more likely to give birth and thats why they received the more vaccines?
# but it should actually be: vaccines received before coming back to birth for those that did.
# actually: cannot compare, since its biased if I count like this.
# differently for both groups: before coming back to those that did but overall for those that didnt.
# year in which they received primer?
efficient_unambigous$year_primer <- as.factor(year(efficient_unambigous$Primer))
model_year <- glm(birth_again ~ year_primer, data = efficient_unambigous, family = binomial)
summary(model_year)
# absolutely no effect

# how long on average were they infertile for?
efficient_one<- efficient_one %>% 
  mutate(time_infertility = case_when(
    !is.na(Foal_DOB_P_3yr) ~ Foal_DOB_P_3yr,
    !is.na(Foal_DOB_P_4yr) ~ Foal_DOB_P_4yr,
    !is.na(Foal_DOB_P_5yr) ~ Foal_DOB_P_5yr,
    !is.na(Foal_DOB_P_6yr) ~ Foal_DOB_P_6yr,
    !is.na(Foal_DOB_P_7yr) ~ Foal_DOB_P_7yr,
    !is.na(Foal_DOB_P_8yr) ~ Foal_DOB_P_8yr,
    !is.na(Foal_DOB_P_9yr) ~ Foal_DOB_P_9yr,
    !is.na(Foal_DOB_P_10yr) ~ Foal_DOB_P_10yr))


# days from primer to first foal
pzp <- pzp %>% 
  mutate(days_primer_to_firstfoal = as.numeric( first_foal_after_treatment - Primer))
efficient_one <- efficient_one %>%  mutate(time_of_infertility =
                              as.numeric(time_infertility - Primer))

str(efficient_one$time_of_infertility)
mean(efficient_one$time_of_infertility)/365
sd(efficient_one$time_of_infertility)/365 
# avg time of infertility: 5 years 7 months, sd 0.9779
max(efficient_one$time_of_infertility)/365  # 6.95
min(efficient_one$time_of_infertility)/365 # 4.05
efficient_one$vaccines_number <- rowSums(!is.na(efficient_one[, c(5:12)]))
efficient_one$age_at_primer <- as.numeric(efficient_one$Primer - efficient_one$birth_date)
mean(efficient_one$age_at_primer)/365 # 5.12 years old

(plot_vaccines <- ggplot (efficient_one, aes( x = vaccines_number, y = time_of_infertility))+ geom_point())
# doesnt make too much sense as dont know if they got the vacccines before or after

efficient_one <- efficient_one %>%
  mutate(infertility_years = round(time_of_infertility / 365))  

efficient_one <- efficient_one %>%
  group_by(infertility_years) %>%
  mutate(point_position = seq(1, n(), length.out = n()))  # Evenly distribute points

efficient_one <- efficient_one %>%
  mutate(infertility_years = factor(infertility_years))

#  Plot with bars and evenly distributed points
(plot_vaccines_2 <- ggplot(efficient_one, aes(x = infertility_years)) +
    # Bar plot layer
    geom_bar(fill = "lightgray", color = "black") +
    # Overlay points evenly distributed along a vertical line within the bar
    geom_point(aes(y = point_position, color = factor(vaccines_number)), 
               size = 3, position = position_nudge(x = 0)) +  # Adjust point size and nudge
    scale_color_discrete(name = "Number of Vaccines") +  # Color by number of vaccines
    labs(x = "\nInfertile Years", y = "Count\n") +  # Adjust labels
    theme_minimal() ) 
# again, this plot makes no sense

efficient_one <- efficient_one%>%
  mutate(age_in_years_at_primer = round(age_at_primer / 365))  
efficient_one<-  efficient_one %>%
  mutate(age_category = factor(age_in_years_at_primer))  

efficient_one <- efficient_one %>%
  group_by(infertility_years) %>%
  mutate(point_position = seq(1, n(), length.out = n()))  

(plot_vaccines_2 <- ggplot(efficient_one, aes(x = infertility_years)) +
    geom_bar(fill = "lightgray", color = "black") +
    geom_point(aes(y = point_position, color = age_category), 
               size = 3, position = position_nudge(x = 0)) +  
    scale_color_discrete(name= "Age in Years at Primer") +  
    labs(x = "Infertile Years", y = "Count") +  
    theme_minimal())

# Function to calculate the longest consecutive "NP" in a vector
longest_consecutive_np <- function(row) {
  # Convert the row to a character vector to handle cases like factors
  row <- as.character(row)
  # Find consecutive "NP" values
  rle_result <- rle(row == "NP")
  # Return the length of the longest run of TRUE (where "NP" occurs)
  max(rle_result$lengths[rle_result$values == TRUE], na.rm = TRUE)}

str(efficient_zero[, 30:39])

efficient_zero$infertile_years <- apply(efficient_zero[, 30:39], 1, longest_consecutive_np)
efficient_zero$vaccines_number <- rowSums(!is.na(efficient_two[, c(5:12)]))
efficient_zero$age_at_primer <- as.numeric(efficient_zero$Primer - efficient_zero$birth_date)
mean(efficient_zero$age_at_primer)/365  # 6.1 years old
sd(efficient_zero$age_at_primer)/365
min(efficient_zero$age_at_primer)/365
max(efficient_zero$age_at_primer)/365
(plot_vaccines <- ggplot (efficient_zero, aes( x = age_at_primer/365, y = infertile_years))+ geom_point())
# no but this makes no sense this is just the length of data we have...



# if we just look at the "standard treatment"----
# filter horses who got 2 or 3 vaccines
reversibility_data <- pzp %>% filter(!is.na(Primer) & !is.na(Booster1) 
                                     & is.na (Booster3) & is.na(Booster4)) %>% 
  mutate(which_vaccines = case_when(!is.na(Booster2) ~ "3",
                                    is.na(Booster2) ~ "2")) %>% 
  select (-Std_num, -Booster3, -Booster4, - DOB_latest_foal, -DOB_foal_year_before, -primer_on_time, -booster_on_time, 
          - fertility, -efficiency_year1)

(vaccines_summary <- reversibility_data %>% 
    group_by(which_vaccines) %>% 
    summarise(count = length(which_vaccines)))
# 60 got 2 vaccines, 86 got 3

str(reversibility_data[,25:34])
# add how many years of data we have
reversibility_data <- reversibility_data %>%
  mutate(years_of_data = 
           ifelse(!is.na(Primer_again), 
      as.numeric(format(Primer_again, "%Y")) - as.numeric(format(Primer, "%Y")),
      apply(select(., 25:34), 1, function(row) {
        # if they didnt start treatment again - 
        stop_index <- which(is.na(row) | row == "OUT")[1]
        if (is.na(stop_index)) {10 }
        # Maximum years if no NA or "OUT" is found 
        else {stop_index - 1} 
      })))# Count up to the first occurrence

# remove observations of horses with no data 
reversibility_data <- filter(reversibility_data, years_of_data != "0")

# variable for whether ever observed to give birth again (logi)
reversibility_data <- reversibility_data %>% 
  mutate(birth_again = sapply(1:nrow(.), function(i) {
    row <- as.vector(unlist(select(.[i,], 11:20)))
    booster_value <- if(which_vaccines[i] ==3) 
    {Booster2[i] } 
    else if (which_vaccines[i] ==2) {Booster1[i]}
    else {NA}
    any(row > booster_value, na.rm = TRUE)}))

# add when the first recorded birth post-treatment was if any
reversibility_data <- reversibility_data %>%
  mutate(date_birth_again = as.Date(sapply(1:nrow(.), function(i) {
    row <- as.vector(unlist(select(.[i, ], 11:20)))
    booster_value <- if (which_vaccines[i] == 3) { Booster2[i]} 
    else if (which_vaccines[i] == 2) 
    {Booster1[i]} 
    else { NA}
    if (birth_again[i]) 
    # this means if birth again is TRUE
    {return(min(row[!is.na(row) & row > booster_value]))} 
    else 
    {return(NA)  # If birth_again is FALSE, return NA
    }}))) 

str(reversibility_data)
(reversible_summary <- reversibility_data %>% 
    group_by(which_vaccines, birth_again) %>% 
    summarise(count = length(Name)))

reversibility_2vacc <- reversibility_data %>% filter(which_vaccines == 2)
reversibility_3vacc <- reversibility_data %>% filter(which_vaccines == 3)

# 2 vaccines
(reversibility_2vacc_sum_years <- reversibility_2vacc %>% 
    group_by(years_of_data) %>% 
    summarise(count = length (Name)))

(reversibility_2vacc_plot <- ggplot(reversibility_2vacc,
                                    aes(x = years_of_data, 
                                        fill = birth_again))+
    geom_histogram(binwidth = 0.5)+
    scale_fill_manual(values = c("#CD6600", "#1C86EE"), labels = c("No", "Yes"))+
    labs(x = "\nYears of data available", y = "Number of individuals\n")+
    ggtitle("a) Primer and one booster\n")+
    scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9))+
    guides(fill=guide_legend(title="Gave birth post-treatment"))+
    scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9))+
    theme_classic()+
    theme(legend.position = "none"))
ggsave("pzp/plots/years_vs_reversibility_2yrs.png", dpi = 600)


# 3 vaccines
(reversibility_3vacc_sum_years <- reversibility_3vacc %>% 
    group_by(years_of_data) %>% 
    summarise(count = length (Name)))

(reversibility_3vacc_plot <- ggplot(reversibility_3vacc,
                                    aes(x = years_of_data, 
                                        fill = birth_again))+
    scale_fill_manual(values = c("#CD6600", "#1C86EE"), labels = c("No", "Yes"))+
    labs(x = "\nYears of data available", y = "Number of individuals\n")+
    ggtitle ("b) Primer and two boosters\n")+
    scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9))+
    guides(fill=guide_legend(title="Gave birth post-treatment"))+
    theme_classic()+
    theme(legend.position = "none")+
    geom_histogram(binwidth = 0.5))
ggsave("pzp/plots/years_vs_reversibility_3yrs.png", dpi = 600)

# side by side plot 
reversibility_combined <- grid.arrange(reversibility_2vacc_plot, reversibility_3vacc_plot, nrow = 1, ncol = 2)
ggsave("pzp/plots/reversible_vs_years.png", plot = reversibility_combined, dpi = 600)

# unnecessary now, check later what this is, i forgot----
pbb <-  pzp %>%  filter(!is.na(Primer)& !is.na(Booster1) & !is.na(Booster2)
                        & is.na(Booster3) & is.na(Booster4))

pbb <- pbb %>%
  mutate(birth_again = sapply(1:nrow(.), function(i) {
    # Extract the row as a vector of dates
    row <- as.vector(unlist(select(.[i, ], 14:23)))
    # Check if any non-NA value is greater than Booster2
    any(row > Booster2[i], na.rm = TRUE)}))

pbb_summary <- pbb %>% 
  group_by(birth_again) %>% 
  summarise(count = length(birth_again))
pbb_summary

pbb <- pbb %>%
  mutate(years_of_data = apply(select(., 31:39), 1, function(row) {
      # Find the position of the first NA or "OUT"
      stop_index <- which(is.na(row) | row == "OUT")[1]
      # Count up to the first occurrence
      return(stop_index - 1)}))
str(pbb[, 31:39])

pbb <- pbb %>% filter(years_of_data > 0)

# graphs
# is there a relationship between years of data and whether they come back to give birth or not?
# generally weird pattern.
# Age? I looked at this already but in the other group

# models on reversibility, currently with stricter data----

# 1- years of data
years_model <- glm( birth_again_strict ~ years_of_data, family = binomial, data = reversibility_data)
summary(years_model)
# + - the more years, the more likely it is to be true (0/1 in R)
# more likely to be false if fewer years of data, makes sense
# aic 87

# 2 - age at primer
reversibility_data <- mutate(reversibility_data, age_at_primer = as.numeric((Primer-birth_date)/365))
age_model <- glm(birth_again_strict ~age_at_primer, family = binomial, data = reversibility_data) 
summary(age_model)

# 3 - both
both_model <- glm(birth_again_strict ~ age_at_primer + years_of_data, family = binomial, data = reversibility_data) 
summary(both_model)
# still better with explaining just with the years of data

# 4 - years of data, no of vaccines
vaccines_model <- glm(birth_again_strict ~ years_of_data + which_vaccines, family = binomial, data = reversibility_data)     
summary(vaccines_model)
# not better

# try a random effect - not ok
# null 
null_model <- glm(birth_again_strict ~1, family = binomial, data = reversibility_data)
summary(null_model) # good its higher aic

# how long on average to first foal? From the end of treatment
reversibility_data <- mutate(reversibility_data,
                             time_to_foal = case_when(
                               which_vaccines == 2 ~ as.numeric((date_birth_again - Booster1)/365),
                               which_vaccines == 3 ~ as.numeric((date_birth_again - Booster2) /365)))

# average for everyone 
mean(reversibility_data$time_to_foal, na.rm = TRUE) # 1.5 years after last vaccine
# ok, so some seem to come back because vaccination was not efficient in the first place.
# they give birth immediately after B2, which means P and B didnt work.

# so what if we change the condition? Not just birth after B2 / B1, but birth after at least 1 infertile year.

# reversibility, standard treatment, stricter criteria ----

# did they give birth again, after at least 1 infertile year?
reversibility_data <- reversibility_data %>% 
  mutate(birth_again_strict = sapply(1:nrow(.), function(i) {
    row <- as.vector(unlist(select(.[i,], 11:20)))
    booster_value <- if(which_vaccines[i] ==3) 
    {Booster2[i] } 
    else if (which_vaccines[i] ==2) {Booster1[i]}
    else {NA}
    any(row > (booster_value+545), na.rm = TRUE) }))
    # the recorded birth has to be at least 1.5 years after the last vaccine
    # this ensures that there was at least 1 infertile year after the last vaccine was received
  

reversibility_data <- reversibility_data %>%
  mutate(birth_strict = as.Date(sapply(1:nrow(.), function(i) {
    row <- as.vector(unlist(select(.[i, ], 11:20)))
    booster_value <- if (which_vaccines[i] == 3) { Booster2[i]} 
    else if (which_vaccines[i] == 2) 
    {Booster1[i]} 
    else { NA}
    if (birth_again_strict[i]) 
    {return(min(row[!is.na(row) & row > (booster_value+545) ]))} 
    else {return(NA)}  }))) 
    # If birth_again is FALSE, return NA
  

reversibility_min2yrs <- reversibility_data %>% 
  filter(years_of_data != 1)
View(reversibility_min2yrs)

str(reversibility_data)
(reversible_summary_2 <- reversibility_min2yrs %>% 
    group_by(which_vaccines, birth_again_strict) %>% 
    summarise(count = length(Name)))
# 2 vaccines: 12 didnt come back, 9 did. 
# 3 vaccines: 53 didnt come back, 15 did.

reversibility_2vacc_strict <- reversibility_min2yrs %>% filter(which_vaccines == 2)
reversibility_3vacc_strict <- reversibility_min2yrs %>% filter(which_vaccines == 3)

# 2 vaccines
(reversibility_2vacc_sum_years_strict <- reversibility_2vacc_strict %>% 
    group_by(years_of_data) %>% 
    summarise(count = length (Name)))

(reversibility_2vacc_plot_strict<- ggplot(reversibility_2vacc_strict,
                                          aes(x = years_of_data, 
                                              fill = birth_again_strict))+
    geom_histogram(binwidth = 0.5)+
    scale_fill_manual(values = c("#CD6600", "#1C86EE"), labels = c("No", "Yes"))+
    labs(x = "\nYears of data available", y = "Number of individuals\n")+
    ggtitle("a) Primer and one booster\n")+
    
    guides(fill=guide_legend(title="Gave birth post-treatment"))+
    theme_classic()+
     theme(legend.position = "none")+
    scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9)))
ggsave("pzp/plots/years_vs_reversibility_2yrs.png", dpi = 600)


# 3 vaccines
(reversibility_3vacc_sum_years_strict <- reversibility_3vacc_strict %>% 
    group_by(years_of_data) %>% 
    summarise(count = length (Name)))

(reversibility_3vacc_plot_strict <- ggplot(reversibility_3vacc_strict,
                                    aes(x = years_of_data, 
                                        fill = birth_again_strict))+
    scale_fill_manual(values = c("#CD6600", "#1C86EE"), labels = c("No", "Yes"))+
    labs(x = "\nYears of data available", y = "Number of individuals\n")+
    ggtitle ("b) Primer and two boosters\n")+
    scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9))+
    guides(fill=guide_legend(title="Gave birth post-treatment"))+
    theme_classic()+
    theme(legend.position = "none")+
    geom_histogram(binwidth = 0.5))
ggsave("pzp/plots/years_vs_reversibility_3yrs.png", dpi = 600)

reversibility_combined_strict <- grid.arrange(reversibility_2vacc_plot_strict, reversibility_3vacc_plot_strict, nrow = 1, ncol = 2)
ggsave("pzp/plots/reversible_vs_years_strict.png", plot = reversibility_combined_strict, dpi = 600)

reversibility_data <- mutate(reversibility_data,
                             time_to_foal_strict = case_when(
                               which_vaccines == 2 ~ as.numeric((birth_strict - Booster1)/365),
                               which_vaccines == 3 ~ as.numeric((birth_strict - Booster2) /365)))

# average for everyone 
mean(reversibility_data$time_to_foal_strict, na.rm = TRUE)
range(reversibility_data$time_to_foal_strict, na.rm = TRUE)
sd(reversibility_data$time_to_foal_strict, na.rm = TRUE)
# trying to plot data based on time since treatment  ----
# first - horses with 3 vaccines
# new df: each year, each horse, pregnant or not (if present)
# fix/ check year of foal:
# is it the year after B2 in this case? or year after primer?
reversibility_longer <- reversibility_3vacc_strict %>% 
  pivot_longer( cols= c(25:34), names_to = "Year_of_foal", values_to = "Pregnancy_status" )
#reversibility_longer <- reversibility_longer %>% 
#  filter(!is.na(Pregnancy_status)) %>% 
#  filter(Pregnancy_status != "NK") %>% 
#  filter(Pregnancy_status != "OUT")
nrow

reversibility_longer <- reversibility_longer %>% 
  filter(!is.na(Pregnancy_status)) %>% 
  filter(Pregnancy_status != "OUT" ) %>% 
  filter(Pregnancy_status != "NK")

# double check year 6
(year6_find_the_issue <- reversibility_3vacc_strict %>% 
  group_by(Foal_Status_P_6yr) %>% 
  summarise(count = length(Name)))

(reversibility_summary_attempt <- reversibility_longer %>% 
    group_by(Year_of_foal, Pregnancy_status ) %>% 
    summarise(count = sum(!is.na(Pregnancy_status))))

# try excluding those who were treated again
reversibility_longer <- reversibility_longer %>%
  mutate(Year_of_foal = as.numeric(str_extract(Year_of_foal, "\\d+")))
# redo calculations as i am not sure - ??

# reversibility_longer <- reversibility_longer %>%
#  filter(is.na(Primer_again) | (Year_of_foal ) < (as.numeric(format(Primer_again, "%Y")) - as.numeric(format(Primer, "%Y"))))
# this seems to cause the issue - but I dont understand why. 

reversibility_longer <- reversibility_longer %>% 
  mutate (years_between_primer_and_p2 = ifelse(is.na(Primer_again), NA, 
         (as.numeric(format(Primer_again, "%Y"))) - (as.numeric(format(Primer, "%Y"))))
         )

reversibility_longer <- reversibility_longer %>% 
  filter ((is.na (Primer_again)) | (!is.na(Primer_again) & Year_of_foal < years_between_primer_and_p2))


# an attempt at plot but i dont acc need the summary for this...
(plot_by_year <- ggplot(reversibility_longer,
                        aes(x = Year_of_foal, 
                            fill = Pregnancy_status))+
    scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9))+
    labs(x = "Year", y = "Number of individuals")+
    scale_fill_manual(values = c("#CD6600", "#1C86EE"), labels = c("No foal", "Foal"))+
    geom_histogram(stat = "count")+
    theme_classic())  

# percentage attempt 

# Compute proportions within each year
reversibility_longer_percent <- reversibility_longer %>%
  group_by(Year_of_foal) %>%
  mutate(total = n()) %>%
  group_by(Year_of_foal, Pregnancy_status) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(percent = 100 * count / sum(count))
reversibility_longer_percent

reversibility_longer_percent$year_of_foal <- as.factor(reversibility_longer_percent$Year_of_foal)

(plot_by_year_3vacc <- ggplot(reversibility_longer_percent, 
                        aes(x = factor(Year_of_foal), 
                            y = percent, 
                            fill = Pregnancy_status))+
    scale_x_discrete(labels = c("1\n (67)", "2\n(61)", "3\n(48)", 
                                "4\n (30)", "5\n (17)", "6\n (12)", 
                                "7\n (4)", "8\n(3)", "9\n(1)"))+
    # scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9))+
    labs(x = "\nYear (sample size)", y = "% of all individuals with data for each year\n", fill = "Status")+
    geom_col(position = "fill") +  # Fill makes bars 100% stacked
    scale_fill_manual(values = c("orange", "skyblue"), labels = c("Not foaling", "Foaling")) +
   # scale_y_continuous(labels = scales::percent_format(scale = 1)) +  # Convert to percentage format
    theme_classic()+
    theme(legend.title = element_blank()))


ggsave("pzp/plots/returns_in_each_year_3vacc.png", dpi = 600)
# but have to somehow 1) add sample size
# b) make it clear that for some horses we simply do not have the data for 5 etc years,
# so it might be a false impression that they do not come back.
# AND ALSO: check what was the deal with those who got B3 and B4 BEFORE they got it
# or add that they were treated again but maybe were the ones for whom
# treatment was more reversible.

# ok, but also: if they got primer again, exclude them from the years after they got the P again.
# maybe if the date recorded for something is later than the p + years of data then exclude? 
# i dont knooow

reversibility_longer_2vacc <- reversibility_2vacc_strict %>% 
  pivot_longer( cols= c(25:34), names_to = "Year_of_foal", values_to = "Pregnancy_status" )

reversibility_longer_2vacc <- reversibility_longer_2vacc %>% 
  filter(!is.na(Pregnancy_status)) %>% 
  filter(Pregnancy_status != "NK") %>% 
  filter(Pregnancy_status != "OUT")

reversibility_longer_2vacc <- reversibility_longer_2vacc %>% 
  mutate (years_between_primer_and_p2 = ifelse(is.na(Primer_again), NA, 
                                               (as.numeric(format(Primer_again, "%Y"))) - (as.numeric(format(Primer, "%Y"))))
  )

reversibility_longer_2vacc <- reversibility_longer_2vacc %>% 
  filter ((is.na (Primer_again)) | (!is.na(Primer_again) & Year_of_foal < years_between_primer_and_p2))


(reversibility_summary_attempt <- reversibility_longer %>% 
    group_by(Year_of_foal, Pregnancy_status ) %>% 
    summarise(count = sum(!is.na(Pregnancy_status))))
View(reversibility_summary_attempt)
reversibility_summary_attempt <- reversibility_summary_attempt %>% 
  drop_na() %>% 
  filter(Pregnancy_status != "OUT") %>% 
  filter(Pregnancy_status != "NK")

# an attempt at plot but i dont acc need the summary for this...
(plot_by_year <- ggplot(reversibility_longer_2vacc,
                        aes(x = Year_of_foal, 
                            fill = Pregnancy_status))+
    scale_fill_manual(values = c("#CD6600", "#1C86EE"), labels = c("Not pregnant", "Pregnant"))+
    geom_histogram(stat = "count")+
    theme_classic())  

# percentage attempt 

# Compute proportions within each year
reversibility_longer_percent_2vacc <- reversibility_longer_2vacc %>%
  group_by(Year_of_foal) %>%
  mutate(total = n()) %>%
  group_by(Year_of_foal, Pregnancy_status) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(percent = count / sum(count) * 100)  # Compute percentages

reversibility_longer_percent_2vacc$Year_of_foal <- as.factor(reversibility_longer_percent_2vacc$Year_of_foal)
reversibility_longer_percent_2vacc

# Plot as 100% stacked bars
(plot_by_year_2vacc <- ggplot(reversibility_longer_percent_2vacc, 
                        aes(x = Year_of_foal, 
                            y = percent, 
                            fill = Pregnancy_status)) +
    scale_x_discrete(labels = c("1\n (20)", "2\n(17)", "3\n(15)", 
                                "4\n (11)", "5\n (7)", "6\n (6)", 
                                "7\n (2)", "8\n(1)", "9\n(1)"))+
    geom_col(position = "fill") +  # Fill makes bars 100% stacked
    labs(x = "\nYear (sample size)", y = "% of all individuals with data for each year\n", fill = "Status")+
    scale_fill_manual(values = c("orange", "skyblue"), labels = c("Not foaling", "Foaling")) +
    # scale_y_continuous(labels = scales::percent_format(scale = 1)) +  # Convert to percentage format
    theme_classic()+
    theme(legend.title = element_blank()))

ggsave("pzp/plots/returns_in_each_year_2vacc.png", dpi = 600)


# ok, but also: if they got primer again, exclude them from the years after they got the P again.
# maybe if the date recorded for something is later than the p + years of data then exclude? 
# i dont knooow

reversibility_longer_2vacc <- reversibility_longer_2vacc %>%
  mutate(Year_of_foal = as.numeric(str_extract(Year_of_foal, "\\d+")))
# redo calculations as i am not sure

reversibility_longer_2vacc <- reversibility_longer_2vacc %>%
  filter(is.na(Primer_again) | (Year_of_foal - 1) <= (as.numeric(format(Primer_again, "%Y")) - as.numeric(format(Primer, "%Y"))))







# horses w more than 3 vaccines----
irregular_treatment <- pzp %>% filter(!is.na(Primer) &
                                        !is.na(Booster1)&
                                        !is.na(Booster2) ) %>% 
 filter (!is.na(Booster3) | (!is.na(Booster4)))
nrow(irregular_treatment)  

reversibility_longer_irregular <- irregular_treatment %>% 
  pivot_longer( cols= c(30:39), names_to = "Year_of_foal", values_to = "Pregnancy_status" )
reversibility_longer_irregular <- reversibility_longer_irregular %>% 
  filter(!is.na(Pregnancy_status)) %>% 
  filter(Pregnancy_status != "NK") %>% 
  filter(Pregnancy_status != "OUT")

reversibility_longer_irregular <- reversibility_longer_irregular %>%
  mutate(Year_of_foal = as.numeric(str_extract(Year_of_foal, "\\d+")))

reversibility_longer_irregular_percent <- reversibility_longer_irregular %>%
  group_by(Year_of_foal) %>%
  mutate(total = n()) %>%
  group_by(Year_of_foal, Pregnancy_status) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(percent = 100 * count / sum(count))
reversibility_longer_irregular_percent

reversibility_longer_irregular_percent$year_of_foal <- as.factor(reversibility_longer_irregular_percent$Year_of_foal)

(plot_by_year_irregular <- ggplot(reversibility_longer_irregular_percent, 
                              aes(x = factor(Year_of_foal), 
                                  y = percent, 
                                  fill = Pregnancy_status))+
    scale_x_discrete(labels = c("1\n (19)", "2\n(18)", "3\n(20)", 
                                "4\n (17)", "5\n (13)", "6\n (12)", 
                                "7\n (9)", "8\n(7)", "9\n(2)"))+
    # scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9))+
    labs(x = "\nYear (sample size)", y = "% of all individuals with data for each year\n")+
    geom_col(position = "fill") +  # Fill makes bars 100% stacked
    scale_fill_manual(values = c("#CD6600", "#1C86EE"), labels = c("Not pregnant", "Pregnant")) +
    # scale_y_continuous(labels = scales::percent_format(scale = 1)) +  # Convert to percentage format
    theme_classic())
ggsave("pzp/plots/returns_per_year_irregular.png", dpi = 600)

# look at a 4 year period after treatment
fouryear_subset <- reversibility_data %>% 
  filter(!is.na(Booster2)) %>% 
  filter(!is.na(Foal_Status_P_5yr) & Foal_Status_P_5yr != "NK" &  Foal_Status_P_5yr != "OUT" )
nrow(fouryear_subset)
View(fouryear_subset)


View(pzp)
pzp_summary <- pzp %>% 
  mutate(primer_year = year(Primer)) %>% 
  group_by(primer_year) %>% 
  summarise(count =n())

pzp_summary

ggplot(pzp_summary, aes(x = primer_year, y = count)) +
  geom_col(fill = "steelblue") +  
  labs(
       x = "Year",
       y = "Number of horses") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(pzp_summary$primer_year), max(pzp_summary$primer_year), 1))
