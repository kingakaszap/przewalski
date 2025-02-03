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


# models ? / chi squared -----
# check if everything actually belongs here
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
