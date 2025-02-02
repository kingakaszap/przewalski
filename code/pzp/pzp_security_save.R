# 2024 june

# libraries and data import----

library(tidyverse)
library(readxl)
library(ggrepel)
library(lubridate)
library(gridExtra)

pzp <- read_excel("data/pzp/pzpdata.xlsx")
pzp <- mutate_if(pzp, is.POSIXct, as.Date)
# calculating numbers and efficiency for the whole dataset ----

# year 1
# had P and B on time, and we have data for year 1 pregnancy status.

group_1 <- pzp %>% 
  subset(Primer <= efficiency_primer & Booster1 <= efficiency_booster & 
           (Foal_Status_P_1yr != "OUT" & Foal_Status_P_1yr !="NK" &!is.na(Foal_Status_P_1yr))) 
nrow(group_1)
nrow(group_1)/nrow(pzp)

# 56 got both primer & booster on time!

# pregnancy status

summary_ontime_y1<- group_1 %>% 
  group_by(Foal_Status_P_1yr) %>% 
  summarise(count = length(Name)) %>%
  ungroup()
summary_ontime_y1
48/(48+8) 
# 86% effective
8/(48+8)
# 14 % pregnant

group_2 <- pzp %>% 
  subset(Primer <= efficiency_primer & Booster1 > efficiency_booster&
           Foal_Status_P_1yr != "OUT" &Foal_Status_P_1yr !="NK" &!is.na(Foal_Status_P_1yr))
nrow(group_2)/nrow(pzp)
nrow(group_2)
#  31 got  primer on time and booster late

summary_b_late<- group_2 %>% 
  group_by(Foal_Status_P_1yr) %>% 
  summarise(count = length(Name)) %>%
  ungroup()
summary_b_late
19/(19+12) 
# 61.3% effective
12/(19+12)
# 38.7% pregnant

# P and B late
group_3 <- pzp %>% 
  subset(Primer > efficiency_primer & Booster1 > efficiency_booster&
           Foal_Status_P_1yr != "OUT" &Foal_Status_P_1yr !="NK" &!is.na(Foal_Status_P_1yr))
nrow(group_3)
# 34 horses got p & b late

summary_pblate<- group_3 %>% 
  group_by(Foal_Status_P_1yr) %>% 
  summarise(count = length(Name)) %>%
  ungroup()
summary_pblate
19/(19+15) 
# 56 % effective
15/(19+15)
# 44 % pregnant



# divide by exact date for conception or not ----

year1 <-  subset(pzp,  Foal_Status_P_1yr != "OUT" &Foal_Status_P_1yr !="NK" &!is.na(Foal_Status_P_1yr) & !is.na(Booster1))
nrow(year1) # 122

exact_y1 <- subset(year1, (!is.na(DOB_foal_treatment)) 
                   #| (!is.na(Foal_DOB_P_1yr))
)
View(exact_y1)
nrow(exact_y1) 
# 63 horses not bad - change from 80 to 63

exact_y1$Foal_Status_P_1yr<- as.factor(exact_y1$Foal_Status_P_1yr)
str(exact_y1$Foal_Status_P_1yr) #NP or YES - good


# sample sizes in exact group for later years ----

exact_y2 <- exact_y1 %>% filter( Foal_Status_P_2yr != "OUT" &Foal_Status_P_2yr !="NK" &!is.na(Foal_Status_P_2yr))
nrow(exact_y2) # 68 / 51

exact_y3 <- exact_y1 %>% filter( Foal_Status_P_3yr != "OUT" &Foal_Status_P_3yr !="NK" &!is.na(Foal_Status_P_3yr))
nrow(exact_y3) #39 

exact_y4 <- exact_y1 %>% filter( Foal_Status_P_4yr != "OUT" &Foal_Status_P_4yr !="NK" &!is.na(Foal_Status_P_4yr))
nrow(exact_y4) # 41 /30

exact_y5 <- exact_y1 %>% filter( Foal_Status_P_5yr != "OUT" &Foal_Status_P_5yr !="NK" &!is.na(Foal_Status_P_5yr))
nrow(exact_y5) # 23 / 16

exact_y6 <- exact_y1 %>% filter( Foal_Status_P_6yr != "OUT" &Foal_Status_P_6yr !="NK" &!is.na(Foal_Status_P_6yr))
nrow(exact_y6) # 21 /15

exact_y7 <- exact_y1 %>% filter( Foal_Status_P_7yr != "OUT" &Foal_Status_P_7yr !="NK" &!is.na(Foal_Status_P_7yr))
nrow(exact_y7) # 18 /12

# year 1 for exact group----
# summary for whole data set disregarding timing
summary_pby1<- exact_y1 %>% 
  group_by(Foal_Status_P_1yr) %>% 
  summarise(count = length(Name)) %>%
  ungroup()
summary_pby1
45/80 
# 56. 25 % effective
35/80
# 43.75 % pregnant

# new 
45/(18+45) # 71 % effective
18/(18+45) # 29 % pregnant

# including timing

ontime_y1 <- exact_y1 %>% 
  subset(Primer <= efficiency_primer & Booster1 <= efficiency_booster)
View(ontime_y1)
nrow(ontime_y1)
# sample size = 38 /32

summary_y1_ontime<- ontime_y1 %>% 
  group_by(Foal_Status_P_1yr) %>% 
  summarise(count = length(Name)) %>%
  ungroup()
summary_y1_ontime
30/(38) 
# 79% effective
8/(38)
# 21 % pregnant

# new
30/32
# 93.75% effective
2/32
# 6% pregnant 

p_yes_b_no_y1 <- exact_y1 %>% 
  subset(Primer <= efficiency_primer & Booster1 > efficiency_booster)
nrow(p_yes_b_no_y1) # 22 /18

summary_y1_pyesbno<- p_yes_b_no_y1 %>% 
  group_by(Foal_Status_P_1yr) %>% 
  summarise(count = length(Name)) %>%
  ungroup()
summary_y1_pyesbno
10/(22) 
# 45% effective
12/(22)

# new
11/18
# 61% effective
7/18
# 39 % pregnant

p_b_no_y1 <- exact_y1 %>% 
  subset(Primer > efficiency_primer & Booster1 > efficiency_booster)
nrow(p_b_no_y1) # 19/13

summary_y1_pbno<- p_b_no_y1 %>% 
  group_by(Foal_Status_P_1yr) %>% 
  summarise(count = length(Name)) %>%
  ungroup()
summary_y1_pbno
4/(19) 
# 21 % effective
15/(19) 

# new
4/13
# 31% effective
9/13
# 69% pregnant

# plot for year 1 for exact group----
df <- data.frame(
  timing_of_vaccines = c("a","a",
                         "b", "b",
                         "c","c" ),
  pregnant = c("pregnant", "not pregnant", "pregnant", "not pregnant","pregnant", "not pregnant"),
  proportion =c(6, 94, 39, 61, 69, 31)
)

(year1_barplot <- ggplot(df, aes(x = timing_of_vaccines, y = proportion, fill = factor(pregnant))) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.6), color = "black", width = 0.6) +
    scale_fill_manual(values = c("#3CB371", "#9ACD32"),  labels = c("not pregnant", "pregnant")) +
    scale_x_discrete(labels = c("a" = " p and b on time", "b" = "p on time, b late", "c" = "p and b late")) +
    theme_classic() +
    labs(x = "\nTiming of vaccines", y = "Proportion of individuals in category (%)\n") +
    theme(legend.position = "bottom",
          legend.title = (element_blank()),
          axis.title.y = element_text(size = 12), 
          axis.title.x =element_blank(),
          legend.text = element_text(size = 10),
          legend.background = element_rect(colour = "grey"),
          axis.text = element_text(size = 11)))
# Adjusting the y-coordinate to place labels higher
label_y_position <- max(df$proportion) + 5

year1_barplot +
  annotate("text", x = c(1, 2, 3), y = rep(label_y_position, 3),
           label = c("n = 32", "n = 18", "n = 13"), size = 4)

ggsave("przewalski/pzp/year1_barplot.png", width = 8, height = 6, dpi = 300)

# year 2 for exact group ----
# include: got P,B and B2 & data for pregnancy status
primer_booster_y2 <- subset(exact_y2,
                            !is.na(Booster2))
nrow(primer_booster_y2) # 48

summary_pby2<- primer_booster_y2 %>% 
  group_by(Foal_Status_P_2yr) %>% 
  summarise(count = length(Name)) %>%
  ungroup()
summary_pby2
41/(41+7) # 84 % effective

# no booster 2

# y2_b2_no
y2_b2_no <- subset(exact_y2, is.na(Booster2)) 
nrow(y2_b2_no) # 6 horses 
sum_y2_b2_no<- y2_b2_no %>% 
  group_by(Foal_Status_P_2yr) %>% 
  summarise(count = length(Name)) %>%
  ungroup()
sum_y2_b2_no
# 1 pregnant

primer_booster_y2_all <- subset(year1,
                                !is.na(Booster2))
nrow(primer_booster_y2_all) # 96

summary_pby2_all<- primer_booster_y2_all %>% 
  group_by(Foal_Status_P_2yr) %>% 
  summarise(count = length(Name)) %>%
  ungroup()
summary_pby2_all
73/83 # 87% - very similar, sample size:83


# year 3 for exact group----
primer_booster_y3 <- subset(exact_y3,
                            !is.na(Booster2) & is.na(Booster3))
nrow(primer_booster_y3) # 23
summary_pby3<- primer_booster_y3 %>% 
  group_by(Foal_Status_P_3yr) %>% 
  summarise(count = length(Name)) %>%
  ungroup()
summary_pby3
19/(19+4)
# 82.6% effective

# got B3 & B2
booster3 <- exact_y3 %>%
  subset(!is.na(Booster3) & !is.na(Booster2))
nrow(booster3) # only 15 horses got B3

summary_b3 <- booster3 %>% 
  group_by(Foal_Status_P_3yr) %>% 
  summarise(count = length(Name)) %>% 
  ungroup()
summary_b3
13/15
b3_pregnant <- subset(booster3, Foal_Status_P_3yr == "YES")

primer_booster_y3_all <- subset(year1,
                                !is.na(Booster2)& is.na(Booster3))
nrow(primer_booster_y3_all) # 77

summary_pby3_all<- primer_booster_y3_all %>% 
  group_by(Foal_Status_P_3yr) %>% 
  summarise(count = length(Name)) %>%
  ungroup()
summary_pby3_all
41/51 # 80.4% effective, sample size 51

# year 4----
primer_booster_y4 <- exact_y4 %>% 
  subset (is.na(Booster3)& (is.na(Booster4)) & !is.na(Booster2))
nrow(primer_booster_y4) # 16

sum_y4<- primer_booster_y4 %>% 
  group_by(Foal_Status_P_4yr) %>% 
  summarise(count = length(Name)) %>%
  ungroup()
sum_y4
15/16

primer_booster_y4_all <- year1 %>% 
  subset (is.na(Booster3)& (is.na(Booster4)) & !is.na(Booster2))
nrow(primer_booster_y4_all) # 16

sum_y4_all<- primer_booster_y4_all %>% 
  group_by(Foal_Status_P_4yr) %>% 
  summarise(count = length(Name)) %>%
  ungroup()
sum_y4_all
32/35 # 91 % efficient, sample size 35


# year 5----
primer_booster_y5 <- subset(exact_y1,
                            !is.na(Booster1)&
                              (!is.na(Foal_Status_P_5yr) & 
                                 Foal_Status_P_5yr !="NK" & 
                                 Foal_Status_P_5yr != "OUT")&
                              # not treated again
                              (! (!is.na(Primer_again) & Primer_again < Primer + 1825) )&
                              (! (!is.na(Booster1_again) & Booster2_again < Primer + 1825) )& 
                              (! (!is.na(Booster2_again) & Booster2_again < Primer + 1825) )&
                              (! (!is.na(Primer_again3) & Primer_again3 < Primer + 1825) ) 
)
nrow(primer_booster_y5) # 21


# group 2 - no B3 and B4 but y B2
no_b3_y5 <- primer_booster_y5 %>% 
  subset (is.na(Booster3)& (is.na(Booster4)) & !is.na(Booster2))
nrow(no_b3_y5) # 6

sum_y5<- no_b3_y5 %>% 
  group_by(Foal_Status_P_5yr) %>% 
  summarise(count = length(Name)) %>%
  ungroup()
sum_y5


no_b3_y5_all <- year1 %>% 
  subset (is.na(Booster3)& (is.na(Booster4)) & !is.na(Booster2))
nrow(no_b3_y5_all) # 6
sum_y5_all<- no_b3_y5_all %>% 
  group_by(Foal_Status_P_5yr) %>% 
  summarise(count = length(Name)) %>%
  ungroup()
sum_y5_all
13/17 # - 76.5 %

# year 6 -----
primer_booster_y6 <- subset(exact_y1,
                            !is.na(Booster1)&
                              (!is.na(Foal_Status_P_6yr) & 
                                 Foal_Status_P_6yr !="NK" & 
                                 Foal_Status_P_6yr != "OUT")&
                              # not treated again
                              (! (!is.na(Primer_again) & Primer_again < Primer + 2190) )&
                              (! (!is.na(Booster1_again) & Booster2_again < Primer + 2190) )& 
                              (! (!is.na(Booster2_again) & Booster2_again < Primer + 2190) )&
                              (! (!is.na(Primer_again3) & Primer_again3 < Primer + 2190) ) 
)
nrow(primer_booster_y6) # 14


# group 2 - no B3 and B4 but y B2
no_b3_y6 <- primer_booster_y6 %>% 
  subset (is.na(Booster3)& (is.na(Booster4)) & !is.na(Booster2))
nrow(no_b3_y6) # 5

sum_y6<- no_b3_y6 %>% 
  group_by(Foal_Status_P_6yr) %>% 
  summarise(count = length(Name)) %>%
  ungroup()
sum_y6

no_b3_y6_all <- year1 %>% 
  subset (is.na(Booster3)& (is.na(Booster4)) & !is.na(Booster2) & 
            (!is.na(Foal_Status_P_6yr) & 
               Foal_Status_P_6yr !="NK" & 
               Foal_Status_P_6yr != "OUT")&
            # not treated again
            (! (!is.na(Primer_again) & Primer_again < Primer + 2190) )&
            (! (!is.na(Booster1_again) & Booster2_again < Primer + 2190) )& 
            (! (!is.na(Booster2_again) & Booster2_again < Primer + 2190) )&
            (! (!is.na(Primer_again3) & Primer_again3 < Primer + 2190) ) )
nrow(no_b3_y6) # 5

sum_y6_all<- no_b3_y6_all %>% 
  group_by(Foal_Status_P_6yr) %>% 
  summarise(count = length(Name)) %>%
  ungroup()
sum_y6_all
5/10 # 50%

# year 7 -----
primer_booster_y7 <- subset(exact_y1,
                            !is.na(Booster1)&
                              (!is.na(Foal_Status_P_7yr) & 
                                 Foal_Status_P_7yr !="NK" & 
                                 Foal_Status_P_7yr != "OUT")&
                              # not treated again
                              (! (!is.na(Primer_again) & Primer_again < Primer + 2555) )&
                              (! (!is.na(Booster1_again) & Booster2_again < Primer + 2555) )& 
                              (! (!is.na(Booster2_again) & Booster2_again < Primer + 2555) )&
                              (! (!is.na(Primer_again3) & Primer_again3 < Primer + 2555) ) 
)
nrow(primer_booster_y7) # 10


# group 2 - no B3 and B4 but y B2
no_b3_y7 <- primer_booster_y7 %>% 
  subset (is.na(Booster3)& (is.na(Booster4)) & !is.na(Booster2))
nrow(no_b3_y7) # 3

sum_y7<- no_b3_y7 %>% 
  group_by(Foal_Status_P_7yr) %>% 
  summarise(count = length(Name)) %>%
  ungroup()
sum_y7



primer_booster_y8 <- subset(exact_y1,
                            !is.na(Booster1)&
                              (!is.na(Foal_Status_P_8yr) & 
                                 Foal_Status_P_8yr !="NK" & 
                                 Foal_Status_P_8yr != "OUT")&
                              # not treated again
                              (! (!is.na(Primer_again) & Primer_again < Primer + 2920) )&
                              (! (!is.na(Booster1_again) & Booster2_again < Primer + 2920) )& 
                              (! (!is.na(Booster2_again) & Booster2_again < Primer + 2920) )&
                              (! (!is.na(Primer_again3) & Primer_again3 < Primer + 2920) ) 
)
nrow(primer_booster_y8) # 10


# group 2 - no B3 and B4 but y B2
no_b3_y8 <- primer_booster_y8 %>% 
  subset (is.na(Booster3)& (is.na(Booster4)) & !is.na(Booster2))
nrow(no_b3_y8) # 1

sum_y8<- no_b3_y8 %>% 
  group_by(Foal_Status_P_7yr) %>% 
  summarise(count = length(Name)) %>%
  ungroup()
sum_y8


# do they come back to give birth?-----
# need: horses who 
pandb <- pzp %>%  filter(!is.na(Primer)& !is.na(Booster1))
efficient <- pandb %>%
  filter(
    apply(select(., 28:37), 1, function(row) {
      runs <- rle(row == "NP")
      any(runs$values & runs$lengths >= 3) # maybe change to 2 years?
    })
  )
View(efficient)
nrow(efficient)

efficient <- efficient %>%
  mutate(birth_again = apply(select(., 28:37), 1, function(row) {
    # Get run lengths of NA and non-NA sequences
    runs <- rle(row == "NP")
    # Check for at least one run of 3 or more NAs followed by a non-NA
    for (i in seq_along(runs$lengths)) {
      if (runs$values[i] == TRUE && runs$lengths[i] >= 3) { # At least 3 consecutive "NP"
        # Calculate the starting index for the remaining values after the NP run
        start_idx <- sum(runs$lengths[1:i]) + 1
        remaining_values <- row[start_idx:length(row)]
        
        # Check if there's any "YES" in the remaining values, ignoring NA values
        if (any(remaining_values == "YES", na.rm = TRUE)) {
          return(1) # Condition met
        }
      }
    }
    
    return(0) # Condition not met
  }))

efficient <- efficient %>%
  mutate(birth_again = apply(select(., 28:37), 1, function(row) {
    runs <- rle(row == "NP")
    
    # Flag for whether the first condition is met
    first_condition_met <- FALSE
    
    # Check for the first condition: 3 or more consecutive "NP"s followed by a "YES"
    for (i in seq_along(runs$lengths)) {
      if (runs$values[i] == TRUE && runs$lengths[i] >= 3) { # At least 3 consecutive "NP"
        start_idx <- sum(runs$lengths[1:i]) + 1
        remaining_values <- row[start_idx:length(row)]
        
        if (any(remaining_values == "YES", na.rm = TRUE)) {
          first_condition_met <- TRUE
          break
        }
      }
    }
    
    if (first_condition_met) {
      return(1) # First condition met, return 1
    }
    
    # Check for the second condition: 5 or more consecutive "NP"s
    if (any(runs$values & runs$lengths >= 5)) {
      return(0) # Second condition met, return 2
    }
    
    return(2) # Neither condition met, return 0
  }))

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
# year in which they received primer?
efficient_unambigous$year_primer <- as.factor(year(efficient_unambigous$Primer))
model_year <- glm(birth_again ~ year_primer, data = efficient_unambigous, family = binomial)
summary(model_year)
# absolutely no effect

# how long on average were they infertile for?
efficient_one<- efficient_one %>% 
  mutate(time_infertility = case_when(
    !is.na(Foal_DOB_P_4yr) ~ Foal_DOB_P_4yr,
    !is.na(Foal_DOB_P_5yr) ~ Foal_DOB_P_5yr,
    !is.na(Foal_DOB_P_6yr) ~ Foal_DOB_P_6yr,
    !is.na(Foal_DOB_P_7yr) ~ Foal_DOB_P_7yr,
    !is.na(Foal_DOB_P_8yr) ~ Foal_DOB_P_8yr,
    !is.na(Foal_DOB_P_9yr) ~ Foal_DOB_P_9yr,
    !is.na(Foal_DOB_P_10yr) ~ Foal_DOB_P_10yr))


# days from primer to first foal
pzp <- pzp %>% 
  mutate(days_primer_to_firstfoal = 
           as.numeric( first_foal_after_treatment - Primer))
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

# Step 1: Convert 'age at primer' from days to years and round it
efficient_one <- efficient_one %>%
  mutate(infertility_years = round(time_of_infertility / 365))  # Create a new column with rounded age in years

# Step 1: Compute counts for each 'infertile_years'
efficient_one <- efficient_one %>%
  group_by(infertility_years) %>%
  mutate(point_position = seq(1, n(), length.out = n()))  # Evenly distribute points


# Step 2: Make 'age_in_years_at_primer' categorical
efficient_one <- efficient_one %>%
  mutate(infertility_years = factor(infertility_years))

# Step 2: Plot with bars and evenly distributed points
(plot_vaccines_2 <- ggplot(efficient_one, aes(x = infertility_years)) +
    # Bar plot layer
    geom_bar(fill = "lightgray", color = "black") +
    # Overlay points evenly distributed along a vertical line within the bar
    geom_point(aes(y = point_position, color = factor(vaccines_number)), 
               size = 3, position = position_nudge(x = 0)) +  # Adjust point size and nudge
    scale_color_discrete(name = "Number of Vaccines") +  # Color by number of vaccines
    labs(x = "\nInfertile Years", y = "Count\n") +  # Adjust labels
    theme_minimal() ) # Clean theme

# Step 1: Convert 'age at primer' from days to years and round it
efficient_one <- efficient_one%>%
  mutate(age_in_years_at_primer = round(age_at_primer / 365))  # Create a new column with rounded age in years

# Step 2: Make 'age_in_years_at_primer' categorical
efficient_one<-  efficient_one %>%
  mutate(age_category = factor(age_in_years_at_primer))  # Convert to a categorical variable

# Step 3: Compute positions for the points (as before)
efficient_one <- efficient_one %>%
  group_by(infertility_years) %>%
  mutate(point_position = seq(1, n(), length.out = n()))  # Evenly distribute points

# Step 4: Plot with bars and points colored by 'age_category'
(plot_vaccines_2 <- ggplot(efficient_one, aes(x = infertility_years)) +
    # Bar plot layer
    geom_bar(fill = "lightgray", color = "black") +
    # Overlay points colored by 'age_category', evenly distributed within the bar
    geom_point(aes(y = point_position, color = age_category), 
               size = 3, position = position_nudge(x = 0)) +  # Adjust size and positioning
    scale_color_discrete(name= "Age in Years at Primer") +  # Color by 'age_category'
    labs(x = "Infertile Years", y = "Count") +  # Adjust labels
    theme_minimal())

# other group

# Function to calculate the longest consecutive "NP" in a vector
longest_consecutive_np <- function(row) {
  # Convert the row to a character vector to handle cases like factors
  row <- as.character(row)
  # Find consecutive "NP" values
  rle_result <- rle(row == "NP")
  # Return the length of the longest run of TRUE (where "NP" occurs)
  max(rle_result$lengths[rle_result$values == TRUE], na.rm = TRUE)
}

# Apply this function to each row of columns 30-39
efficient_two$infertile_years <- apply(efficient_two[, 30:39], 1, longest_consecutive_np)
efficient_two$vaccines_number <- rowSums(!is.na(efficient_two[, c(5:12)]))
efficient_two$age_at_primer <- as.numeric(efficient_two$Primer - efficient_two$birth_date)
mean(efficient_two$age_at_primer)/365  # 6.1 years old
sd(efficient_two$age_at_primer)/365
min(efficient_two$age_at_primer)/365
max(efficient_two$age_at_primer)/365
(plot_vaccines <- ggplot (efficient_two, aes( x = age_at_primer/365, y = infertile_years))+ geom_point())

mean(efficient_two$infertile_years)
min(efficient_two$infertile_years)
max(efficient_two$infertile_years)
median(efficient_two$infertile_years)
sd(efficient_two$infertile_years)

# Step 1: Compute counts for each 'infertile_years'
efficient_two <- efficient_two %>%
  group_by(infertile_years) %>%
  mutate(point_position = seq(1, n(), length.out = n()))  # Evenly distribute points

# Step 2: Plot with bars and evenly distributed points
(plot_vaccines_2 <- ggplot(efficient_two, aes(x = infertile_years)) +
    # Bar plot layer
    geom_bar(fill = "lightgray", color = "black") +
    # Overlay points evenly distributed along a vertical line within the bar
    geom_point(aes(y = point_position, color = factor(vaccines_number)), 
               size = 3, position = position_nudge(x = 0)) +  # Adjust point size and nudge
    scale_color_discrete(name = "Number of Vaccines") +  # Color by number of vaccines
    labs(x = "\nInfertile Years", y = "Count\n") +  # Adjust labels
    theme_minimal() ) # Clean theme

# Step 1: Convert 'age at primer' from days to years and round it
efficient_two <- efficient_two %>%
  mutate(age_in_years_at_primer = round(age_at_primer / 365))  # Create a new column with rounded age in years

# Step 2: Make 'age_in_years_at_primer' categorical
efficient_two <- efficient_two %>%
  mutate(age_category = factor(age_in_years_at_primer))  # Convert to a categorical variable

# Step 3: Compute positions for the points (as before)
efficient_two <- efficient_two %>%
  group_by(infertile_years) %>%
  mutate(point_position = seq(1, n(), length.out = n()))  # Evenly distribute points

# Step 4: Plot with bars and points colored by 'age_category'
(plot_vaccines_2 <- ggplot(efficient_two, aes(x = infertile_years)) +
    # Bar plot layer
    geom_bar(fill = "lightgray", color = "black") +
    # Overlay points colored by 'age_category', evenly distributed within the bar
    geom_point(aes(y = point_position, color = age_category), 
               size = 3, position = position_nudge(x = 0)) +  # Adjust size and positioning
    scale_color_discrete(name= "Age in Years at Primer") +  # Color by 'age_category'
    labs(x = "Infertile Years", y = "Count") +  # Adjust labels
    theme_minimal())


# if we just look at the "standard treatment"----
# filter horses who got 2 or 3 vaccines
str(pzp)
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

# add how many years of data we have
reversibility_data <- reversibility_data %>%
  mutate(
    years_of_data = ifelse(
      !is.na(Primer_again), 
      as.numeric(format(Primer_again, "%Y")) - as.numeric(format(Primer, "%Y")),
      apply(select(., 25:33), 1, function(row) {
        # if they didnt start treatment again - 
        stop_index <- which(is.na(row) | row == "OUT")[1]
        if (is.na(stop_index)) {
          9 # Maximum years if no NA or "OUT" is found 
          # or maybe this should be 10 years? check
        } else {
          stop_index - 1 # Count up to the first occurrence
        }
      })
    )
  )


str(reversibility_data[, 25:33])

# remove observations of horses with no data 
reversibility_data <- filter(reversibility_data, years_of_data != "0")

reversibility_data$Foal_DOB_P_9yr<- as.Date(reversibility_data$Foal_DOB_P_9yr)
reversibility_data$Foal_DOB_P_10yr<- as.Date(reversibility_data$Foal_DOB_P_10yr)

# variable for whether ever observed to give birth again
reversibility_data <- reversibility_data %>% 
  mutate(birth_again = sapply(1:nrow(.), function(i) {
    row <- as.vector(unlist(select(.[i,], 11:20)))
    booster_value <- if(which_vaccines[i] ==3) 
    {Booster2[i] } 
    else if (which_vaccines[i] ==2) {Booster1[i]}
    else {NA}
    any(row > booster_value, na.rm = TRUE)
  }))
# add when the first recorded birth was if any
reversibility_data <- reversibility_data %>%
  mutate(date_birth_again = as.Date(sapply(1:nrow(.), function(i) {
    row <- as.vector(unlist(select(.[i, ], 11:20)))
    booster_value <- if (which_vaccines[i] == 3) { Booster2[i]} 
    else if (which_vaccines[i] == 2) 
    {Booster1[i]} 
    else { NA}
    if (birth_again[i]) {
      return(min(row[!is.na(row) & row > booster_value]))
    } else {
      return(NA)  # If birth_again is FALSE, return NA
    }
  }))) 

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
ggsave("przewalski/pzp/plots/years_vs_reversibility_2yrs.png", dpi = 600)


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
ggsave("przewalski/pzp/plots/years_vs_reversibility_3yrs.png", dpi = 600)

# side by side plot 
reversibility_combined <- grid.arrange(reversibility_2vacc_plot, reversibility_3vacc_plot, nrow = 1, ncol = 2)
ggsave("przewalski/pzp/plots/reversible_vs_years.png", plot = reversibility_combined, dpi = 600)

# unnecessary now----
pbb <-  pzp %>%  filter(!is.na(Primer)& !is.na(Booster1) & !is.na(Booster2)
                        & is.na(Booster3) & is.na(Booster4))

# how many were observed to give birth once treatment has finished ?
str(pbb)
pbb$Foal_DOB_P_9yr<- as.Date(pbb$Foal_DOB_P_9yr)
pbb$Foal_DOB_P_10yr<- as.Date(pbb$Foal_DOB_P_10yr)

pbb <- pbb %>%
  mutate(birth_again = sapply(1:nrow(.), function(i) {
    # Extract the row as a vector of dates
    row <- as.vector(unlist(select(.[i, ], 14:23)))
    # Check if any non-NA value is greater than Booster2
    any(row > Booster2[i], na.rm = TRUE)
  }))
# pbb <- pbb %>% 
#  filter(!is.na(Foal_Status_P_1yr) & !is.na(Foal_Status_P_2yr) & !is.na(Foal_Status_P_3yr) 
#        & Foal_Status_P_1yr != "OUT" & Foal_Status_P_2yr != "OUT" & Foal_Status_P_3yr != "OUT") 

pbb_summary <- pbb %>% 
  group_by(birth_again) %>% 
  summarise(count = length(birth_again))
pbb_summary

pbb <- pbb %>%
  mutate(
    years_of_data = apply(select(., 31:39), 1, function(row) {
      # Find the position of the first NA or "OUT"
      stop_index <- which(is.na(row) | row == "OUT")[1]
      # Count up to the first occurrence
      return(stop_index - 1)
    })
  )
str(pbb[, 31:39])

pbb <- pbb %>% 
  filter(years_of_data > 0)

# graphs
# is there a relationship between years of data and whether they come back to give birth or not?
# generally weird pattern.
# Age? I looked at this already but in the other group

# models on reversibility----
# 1- years of data
years_model <- glm( birth_again ~ years_of_data, family = binomial, data = reversibility_data)
summary(years_model)
# more likely to be false if fewer years of data, makes sense

# 2 - age at primer
reversibility_data <- mutate(reversibility_data, age_at_primer = as.numeric((Primer-birth_date)/365))
age_model <- glm(birth_again ~age_at_primer, family = binomial, data = reversibility_data) 
summary(age_model)

# 3 - both
both_model <- glm(birth_again ~ age_at_primer + years_of_data, family = binomial, data = reversibility_data) 
summary(both_model)
# still better with explaining just with the years of data

# 4 - years of data, no of vaccines
vaccines_model <- glm(birth_again ~ years_of_data + which_vaccines, family = binomial, data = reversibility_data)     
summary(vaccines_model)
# not better

# try a random effect - not ok
# null 
null_model <- glm(birth_again ~1, family = binomial, data = reversibility_data)
summary(null_model) # good its higher aic

# how long on average to first foal? From the end of treatment
reversibility_data <- mutate(reversibility_data,
                             time_to_foal = case_when(
                               which_vaccines == 2 ~ as.numeric((date_birth_again - Booster1)/365),
                               which_vaccines == 3 ~ as.numeric((date_birth_again - Booster2) /365)))

# average for everyone 
mean(reversibility_data$time_to_foal, na.rm = TRUE)
# ok, so some seem to come back because vaccination was not efficient in the first place.
# they give birth immediately after B2, which means P and B didnt work.

# so what if we change the condition? Not just birth after B2 / B1, but birth after at least 1 infertile year.



# reversibility, standard treatment, stricter criteria ----

reversibility_data <- reversibility_data %>% 
  mutate(birth_again_strict = sapply(1:nrow(.), function(i) {
    row <- as.vector(unlist(select(.[i,], 11:20)))
    booster_value <- if(which_vaccines[i] ==3) 
    {Booster2[i] } 
    else if (which_vaccines[i] ==2) {Booster1[i]}
    else {NA}
    any(row > (booster_value+545), na.rm = TRUE) 
    # the recorded birth has to be at least 1.5 years after the last vaccine
    # this ensures that there was at least 1 infertile year after the last vaccine was received
  }))

reversibility_data <- reversibility_data %>%
  mutate(birth_strict = as.Date(sapply(1:nrow(.), function(i) {
    row <- as.vector(unlist(select(.[i, ], 11:20)))
    booster_value <- if (which_vaccines[i] == 3) { Booster2[i]} 
    else if (which_vaccines[i] == 2) 
    {Booster1[i]} 
    else { NA}
    if (birth_again_strict[i]) {
      return(min(row[!is.na(row) & row > (booster_value+545) ]))
    } else {
      return(NA)  # If birth_again is FALSE, return NA
    }
  }))) 

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
    # theme(legend.position = "none")+
    scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9))
)
ggsave("przewalski/pzp/plots/years_vs_reversibility_2yrs.png", dpi = 600)


# 3 vaccines
(reversibility_3vacc_sum_years_strict <- reversibility_3vacc_strict %>% 
    group_by(years_of_data) %>% 
    summarise(count = length (Name)))

(reversibility_3vacc_plot <- ggplot(reversibility_3vacc_strict,
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
ggsave("przewalski/pzp/plots/years_vs_reversibility_3yrs.png", dpi = 600)

reversibility_longer <- reversibility_3vacc_strict %>% 
  pivot_longer( cols= c(25:34), names_to = "Year_of_foal", values_to = "Pregnancy_status" )

reversibility_longer <- reversibility_longer %>% 
  filter(!is.na(Pregnancy_status)) %>% 
  filter(Pregnancy_status != "NK") %>% 
  filter(Pregnancy_status != "OUT")

(reversibility_summary_attempt <- reversibility_longer %>% 
    group_by(Year_of_foal, Pregnancy_status ) %>% 
    summarise(count = sum(!is.na(Pregnancy_status))))
View(reversibility_summary_attempt)
reversibility_summary_attempt <- reversibility_summary_attempt %>% 
  drop_na() %>% 
  filter(Pregnancy_status != "OUT") %>% 
  filter(Pregnancy_status != "NK")

# try excluding those who were treated again
reversibility_longer <- reversibility_longer %>%
  mutate(Year_of_foal = as.numeric(str_extract(Year_of_foal, "\\d+")))
# redo calculations as i am not sure

reversibility_longer <- reversibility_longer %>%
  filter(is.na(Primer_again) | (Year_of_foal - 1) <= (as.numeric(format(Primer_again, "%Y")) - as.numeric(format(Primer, "%Y"))))

# an attempt at plot but i dont acc need the summary for this...
(plot_by_year <- ggplot(reversibility_longer,
                        aes(x = Year_of_foal, 
                            fill = Pregnancy_status))+
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
  mutate(percent = count / sum(count) * 100)  # Compute percentages

# Plot as 100% stacked bars
(plot_by_year <- ggplot(reversibility_longer_percent, 
                        aes(x = Year_of_foal, 
                            y = percent, 
                            fill = Pregnancy_status)) +
    geom_col(position = "fill") +  # Fill makes bars 100% stacked
    scale_fill_manual(values = c("#CD6600", "#1C86EE"), labels = c("No foal", "Foal")) +
    scale_y_continuous(labels = scales::percent_format(scale = 1)) +  # Convert to percentage format
    theme_classic())

# ok, but also: if they got primer again, exclude them from the years after they got the P again.
# maybe if the date recorded for something is later than the p + years of data then exclude? 
# i dont knooow





reversibility_longer_2vacc <- reversibility_2vacc_strict %>% 
  pivot_longer( cols= c(25:34), names_to = "Year_of_foal", values_to = "Pregnancy_status" )

reversibility_longer_2vacc <- reversibility_longer_2vacc %>% 
  filter(!is.na(Pregnancy_status)) %>% 
  filter(Pregnancy_status != "NK") %>% 
  filter(Pregnancy_status != "OUT")

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
  mutate(percent = count / sum(count) * 100)  # Compute percentages

# Plot as 100% stacked bars
(plot_by_year <- ggplot(reversibility_longer_percent, 
                        aes(x = Year_of_foal, 
                            y = percent, 
                            fill = Pregnancy_status)) +
    geom_col(position = "fill") +  # Fill makes bars 100% stacked
    scale_fill_manual(values = c("#CD6600", "#1C86EE"), labels = c("No foal", "Foal")) +
    scale_y_continuous(labels = scales::percent_format(scale = 1)) +  # Convert to percentage format
    theme_classic())

# ok, but also: if they got primer again, exclude them from the years after they got the P again.
# maybe if the date recorded for something is later than the p + years of data then exclude? 
# i dont knooow

reversibility_longer <- reversibility_longer %>%
  mutate(Year_of_foal = as.numeric(str_extract(Year_of_foal, "\\d+")))
# redo calculations as i am not sure

reversibility_longer <- reversibility_longer %>%
  filter(is.na(Primer_again) | (Year_of_foal - 1) <= (as.numeric(format(Primer_again, "%Y")) - as.numeric(format(Primer, "%Y"))))







# models ? / chi squared -----

chisq_data <- exact_y1 %>% filter (timing == "p_b_yes"| timing == "p_b_no") %>% 
  filter(Foal_Status_P_1yr == "YES"| Foal_Status_P_1yr == "NP")
str(chisq_data$timing)
chisq_data$timing <- as.character(chisq_data$timing)
chisq_data$timing <- as.factor(chisq_data$timing)

(contingency_table <- table(chisq_data$timing, chisq_data$Foal_Status_P_1yr))

(chi_square_test <- chisq.test(contingency_table))

(fisher_test <- fisher.test(contingency_table))


chisq_data_2 <- exact_y1 %>% filter (timing == "p_b_yes"| timing == "p_yes_b_no") %>% 
  filter(Foal_Status_P_1yr == "YES"| Foal_Status_P_1yr == "NP")

str(chisq_data_2$timing)
chisq_data_2$timing <- as.character(chisq_data_2$timing)
chisq_data_2$timing <- as.factor(chisq_data_2$timing)

(contingency_table_2 <- table(chisq_data_2$timing, chisq_data_2$Foal_Status_P_1yr))

(fisher_test_2 <- fisher.test(contingency_table_2))

# descriptive statistics----

# how many were treated in each year

count_by_year <- function(df, column_name, start_year, end_year) {
  # Create a  vector to store the results
  result <- sapply(start_year:end_year, function(year) {
    sum(year(df[[column_name]]) == year, na.rm = TRUE)
  })
  
  names(result) <- start_year:end_year
  
  return(result)
}

(year_counts <- count_by_year(pzp, "Primer", start_year = 2013, end_year = 2023))

# mean age at treatment
pzp$age_at_primer <- as.numeric(pzp$Primer-pzp$birth_date)
mean(pzp$age_at_primer)/365 # 5.65
sd(pzp$age_at_primer)/365
max(pzp$age_at_primer)/365
min(pzp$age_at_primer) /365

exact_y1$age_at_primer <- as.numeric(exact_y1$Primer-exact_y1$birth_date)
mean(exact_y1$age_at_primer)/365 # 7.7
year1$age_at_primer <- as.numeric(year1$Primer-year1$birth_date)
mean(year1$age_at_primer)/365 # 6
