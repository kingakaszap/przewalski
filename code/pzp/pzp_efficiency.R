# libraries and data import----

library(tidyverse)
library(readxl)
library(ggrepel)
library(lubridate)
library(gridExtra)
library(RColorBrewer)

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
# but where did i get this number from??
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
"#1E90FF"
c("#3CB371", "#9ACD32")
(year1_barplot <- ggplot(df, aes(x = timing_of_vaccines, y = proportion, fill = factor(pregnant))) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.6), color = "black", width = 0.6) +
    scale_fill_manual(values = c("#1E90FF", "orange"),  labels = c("Not foaling", "Foaling")) +
    scale_x_discrete(labels = c("a" = " p and b on time", "b" = "p on time, b late", "c" = "p and b late")) +
    theme_classic() +
    labs(x = "\nTiming of vaccines", y = "Proportion of individuals in category (%)\n") +
    theme(legend.position = "bottom",
          legend.title = (element_blank()),
          axis.title.y = element_text(size = 14), 
          axis.title.x =element_blank(),
          legend.text = element_text(size = 14),
          legend.background = element_rect(colour = "grey"),
          axis.text = element_text(size = 14)))
# adjusting the y-coordinate to place labels higher
label_y_position <- max(df$proportion) + 5

year1_barplot +
  annotate("text", x = c(1, 2, 3), y = rep(label_y_position, 3),
           label = c("n = 32", "n = 18", "n = 13"), size = 5)

ggsave("pzp/plots/year1_barplot_orange_green.png", width = 8, height = 6, dpi = 300)

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
nrow(no_b3_y6) # 

sum_y6<- no_b3_y6 %>% 
  group_by(Foal_Status_P_6yr) %>% 
  summarise(count = length(Name)) %>%
  ungroup()
sum_y6 # 5 sample size; 60%

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
nrow(no_b3_y6) 

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

# small timeline graph ----
# x axis: year, y = proprtion of treated mares foaling 
  timeline_data <- read_excel("data/pzp/efficacy_allhorses.xlsx")
str(timeline_data)

timeline_data <- timeline_data %>% mutate(
  total = Foaling + Not_foaling,
  foaling_proportion = Foaling/total,
  notfoaling_proportion = Not_foaling/total
)

(timeline_graph <- ggplot(timeline_data, aes(x = factor (Year)))+
  geom_col(aes(y = foaling_proportion, fill = "Foaling"), position = "dodge", alpha = 0.7)+
  geom_col(aes(y = notfoaling_proportion, fill = "Not foaling"), position = "dodge", alpha = 0.7)+
  geom_line(aes(y = notfoaling_proportion, group = 1), color = "#4F4F4F", size = 1)+
    geom_point(aes(y = notfoaling_proportion), color = "#4F4F4F", size = 3)+
    scale_y_continuous(labels = scales::percent_format(scale = 1)) +
    scale_y_continuous(
      name = "Proportion of mares")+
      scale_fill_manual(values = c("Foaling " = "skyblue", "Not foaling" = "orange"))+
                          theme_classic()
    )

# side by side bars

timeline_data <- read_excel("data/pzp/efficacy_allhorses.xlsx")

# Compute proportions
timeline_data <- timeline_data %>%
  mutate(
    foaling_proportion = Foaling / (Foaling + Not_foaling),
    notfoaling_proportion = Not_foaling / (Foaling + Not_foaling)
  )

# Convert to long format for side-by-side bars
timeline_long <- timeline_data %>%
  pivot_longer(cols = c(foaling_proportion, notfoaling_proportion),
               names_to = "category", values_to = "proportion")

# Rename categories for better legend display
timeline_long$category <- factor(timeline_long$category, 
                                 levels = c("foaling_proportion", "notfoaling_proportion"),
                                 labels = c("Foaling", "Not foaling"))

timeline_long <- timeline_long %>%
  mutate(proportion = proportion * 100)

timeline_data <- timeline_data %>%
  mutate(notfoaling_proportion = notfoaling_proportion * 100)


# Create the plot
timeline_graph <- ggplot() +
  # Side-by-side bars using timeline_long
  geom_col(data = timeline_long, aes(x = factor(Year), y = proportion, fill = category), 
           position = "dodge", alpha = 0.7) +
  
  # Efficacy line + points using timeline_data
  geom_line(data = timeline_data, aes(x = factor(Year), y = notfoaling_proportion, color = "Efficacy", group = 1), 
             linewidth = 0.7) +
  geom_point(data = timeline_data, aes(x = factor(Year), color = "Efficacy",  y = notfoaling_proportion), 
              size = 2.5) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_x_discrete(labels = c("1\n (77)", "2\n(66)", "3\n(51)", 
                              "4\n (35)", "5\n (17)", "6\n (12)", 
                              "7\n (9)", "8\n(4)", "9\n(3)"))+
  scale_fill_manual(values = c("Foaling" = "#1E90FF", "Not foaling" = "orange")) +
  scale_color_manual(name = "Efficacy", values = c("Efficacy" = "#4F4F4F")) +
  labs (x = "\nYears after treatment started (Sample size)", y = "Proportion of mares" )+
  theme_classic()+
  theme(legend.title = element_blank(),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.text = element_text(size = 12)
        )

View(timeline_data)
print(timeline_graph)

ggsave("pzp/plots/yearly_efficacy_smalldata_9years.png", dpi = 600)

timeline_data_1b <- read_excel("data/pzp/efficacy_allhorses.xlsx", sheet = 2)

# Compute proportions
timeline_data_1b <- timeline_data_1b %>%
  mutate(
    foaling_proportion = Foaling / (Foaling + Not_foaling),
    notfoaling_proportion = Not_foaling / (Foaling + Not_foaling)
  )

# Convert to long format for side-by-side bars
timeline_long_1b <- timeline_data_1b %>%
  pivot_longer(cols = c(foaling_proportion, notfoaling_proportion),
               names_to = "category", values_to = "proportion")

# Rename categories for better legend display
timeline_long_1b$category <- factor(timeline_long_1b$category, 
                                 levels = c("foaling_proportion", "notfoaling_proportion"),
                                 labels = c("Foaling", "Not foaling"))

timeline_long_1b <- timeline_long_1b %>%
  mutate(proportion = proportion * 100)
timeline_data_1b <- timeline_data_1b %>%
  mutate(notfoaling_proportion = notfoaling_proportion * 100)
View(timeline_data_1b)
# Create the plot
timeline_graph_1b <- ggplot() +
  # Side-by-side bars using timeline_long
  geom_col(data = timeline_long_1b, aes(x = factor(Year), y = proportion, fill = category), 
           position = "dodge", alpha = 0.7) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  # Efficacy line + points using timeline_data
  geom_line(data = timeline_data_1b, aes(x = factor(Year), y = notfoaling_proportion, color = "Efficacy", group = 1), 
            linewidth = 0.7) +
  geom_point(data = timeline_data_1b, aes(x = factor(Year), color = "Efficacy",  y = notfoaling_proportion), 
             size = 2.5) +
  scale_x_discrete(labels = c("1\n (25)", "2\n(17)", "3\n(14)", 
                              "4\n (11)", "5\n (7)", "6\n (8)", 
                              "7\n (5)", "8\n(1)", "9\n(1)"))+
  
  scale_fill_manual(values = c("Foaling" = "#1E90FF", "Not foaling" = "orange")) +
  scale_color_manual(name = "Efficacy", values = c("Efficacy" = "#4F4F4F")) +
  labs (x = "\nYears after treatment started", y = "Proportion of mares\n" )+
  theme_classic()+
  theme(legend.title = element_blank(),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 12), 
        legend.text = element_text(size = 12))
c("#4F4F4F", "#474747", "#7A7A7A")

print(timeline_graph_1b)

ggsave("pzp/plots/yearly_efficacy_1booster_revised.png", dpi = 600)

# try to do the same (with efficacy line) with stacked bars also!!
# population number ----
nagylista <- read_excel("data/nagylista.xls", 
                        sheet = "Összes-All", col_types = c("text", 
                                                            "text", "text", "text", "date", "text", 
                                                            "text", "text", "text", "text", "text", 
                                                            "text", "date", "text", "date", "text", 
                                                            "text", "text", "text", "text", "date", 
                                                            "text", "skip"))
View(nagylista)
nagylista <- nagylista %>%
  mutate(Birth_year = as.numeric(format(as.Date(Date_of_birth), "%Y")),
         Death_year = ifelse(is.na(Date_of_death), NA, as.numeric(format(as.Date(Date_of_death), "%Y"))))

# Set Death_year to current year if NA (only for defining the range, not modifying the original data)
nagylista$Death_year_temp <- ifelse(is.na(nagylista$Death_year), format(Sys.Date(), "%Y"), nagylista$Death_year)

# Get valid year range
min_year <- min(nagylista$Birth_year, na.rm = TRUE)
max_year <- max(as.numeric(nagylista$Death_year_temp), na.rm = TRUE)

# Generate the year sequence
year_range <- seq(min_year, max_year)

# Function to count horses alive in a given year
get_population <- function(year) {
  year <- as.numeric(year)
  nagylista %>%
    filter(Birth_year <= year & (is.na(Death_year) | Death_year > year)) %>%
    summarise(Total_Horses = n(),
              Males = sum(Gender == 1, na.rm = TRUE),
              Females = sum(Gender == 2, na.rm = TRUE)) %>%
    mutate(Year = year)
}


# Apply function to each year
population_summary <- bind_rows(lapply(year_range, get_population))

# View result
print(population_summary)
View(population_summary)
# WRONG 


# for the whole dataset, same thing as before 
# but only need those who got P & B1& B2
pbb <- pzp %>%  filter (!is.na(Primer)& !is.na(Booster1) & !is.na(Booster2)
                        & is.na (Booster3) & is.na (Booster4))

pbb <- pbb %>% mutate(years_between_primers = round((as.numeric(Primer_again-Primer))/365))
nrow(pbb)
year1_sum <- pbb %>% filter (!is.na(Foal_Status_P_1yr) & Foal_Status_P_1yr != "NK"
                             & Foal_Status_P_1yr != "OUT")
year1_summary <- year1_sum %>% 
  group_by(Foal_Status_P_1yr) %>% 
  summarise(count = length(Name))
year1_summary
# NP 48, yes 19
# this includes those who got vaccines late

year2_sum <- pbb %>% filter (!is.na(Foal_Status_P_2yr) & Foal_Status_P_2yr != "NK"
                             & Foal_Status_P_2yr != "OUT" 
                              & (is.na (years_between_primers) | years_between_primers != 2) )
year2_summary <- year2_sum %>% 
  group_by(Foal_Status_P_2yr) %>% 
  summarise(count = length(Name))
year2_summary
# np 53, yes 8

year3_sum <- pbb %>% filter (!is.na(Foal_Status_P_3yr) & Foal_Status_P_3yr != "NK"
                             & Foal_Status_P_3yr != "OUT"
                             & (is.na (years_between_primers) | years_between_primers > 3))

year3_summary <- year3_sum %>% 
  group_by(Foal_Status_P_3yr) %>% 
  summarise(count = length(Name))
year3_summary
# 40 np, yes 8

year4_sum <- pbb %>% filter (!is.na(Foal_Status_P_4yr) & Foal_Status_P_4yr != "NK"
                             & Foal_Status_P_4yr != "OUT"
                               & (is.na (years_between_primers) | years_between_primers > 4))
year4_summary <- year4_sum %>% 
  group_by(Foal_Status_P_4yr) %>% 
  summarise(count = length(Name))
year4_summary
# 27 np, yes 3

year5_sum <- pbb %>% filter (!is.na(Foal_Status_P_5yr) & Foal_Status_P_5yr != "NK"
                             & Foal_Status_P_5yr != "OUT"
                             & (is.na (years_between_primers) | years_between_primers > 5 )
                             )
year5_summary <- year5_sum %>% 
  group_by(Foal_Status_P_5yr) %>% 
  summarise(count = length(Name))
year5_summary
# 12 np, yes 5

year6_sum <- pbb %>% filter (!is.na(Foal_Status_P_6yr) & Foal_Status_P_6yr != "NK"
                             & Foal_Status_P_6yr != "OUT"
                             & (is.na (years_between_primers) | years_between_primers > 6))
year6_summary <- year6_sum %>% 
  group_by(Foal_Status_P_6yr) %>% 
  summarise(count = length(Name))
year6_summary
View(year6_sum)
# 7 np, yes 5

year7_sum <- pbb %>% filter (!is.na(Foal_Status_P_7yr) & Foal_Status_P_7yr != "NK"
                             & Foal_Status_P_7yr != "OUT"
                             & (is.na (years_between_primers) | years_between_primers > 7))
year7_summary <- year7_sum %>% 
  group_by(Foal_Status_P_7yr) %>% 
  summarise(count = length(Name))
year7_summary
View(year7_sum)
# 4 np 

year8_sum <- pbb %>% filter (!is.na(Foal_Status_P_8yr) & Foal_Status_P_8yr != "NK"
                             & Foal_Status_P_8yr != "OUT"
                             & (is.na (years_between_primers) | years_between_primers > 8))
year8_summary <- year8_sum %>% 
  group_by(Foal_Status_P_8yr) %>% 
  summarise(count = length(Name))
year8_summary
View(year8_sum)
# np 2, yes 1

year9_sum <- pbb %>% filter (!is.na(Foal_Status_P_9yr) & Foal_Status_P_9yr != "NK"
                             & Foal_Status_P_9yr != "OUT"
                             & (is.na (years_between_primers) | years_between_primers > 9))
year9_summary <- year9_sum %>% 
  group_by(Foal_Status_P_9yr) %>% 
  summarise(count = length(Name))
year9_summary
# 1, np
View(year9_sum)

year10_sum <- pbb %>% filter (!is.na(Foal_Status_P_10yr) & Foal_Status_P_10yr != "NK"
                             & Foal_Status_P_10yr != "OUT"
                             & (is.na (years_between_primers) | years_between_primers > 10))
year10_summary <- year10_sum %>% 
  group_by(Foal_Status_P_10yr) %>% 
  summarise(count = length(Name))
year10_summary
# no horses


# check if they were treated again tho INDIVIDUALLY !!!
# they are not in reversibility data

# 5 np, yes 1

year9_sum <- pbb %>% filter (!is.na(Foal_Status_P_9yr) & Foal_Status_P_9yr != "NK"
                             & Foal_Status_P_9yr != "OUT")
year9_summary <- year9_sum %>% 
  group_by(Foal_Status_P_9yr) %>% 
  summarise(count = length(Name))
year9_summary

# 3 np


# Create the dataset
foaling_data <- data.frame(
  Year = factor(1:9),  # Keep Year as a factor for discrete x-axis
  Foaling = c(19, 8, 8, 3, 5, 5, 0, 1, 0),
  Not_foaling = c(48, 53, 40, 27, 12, 7, 4, 2, 1)
)

# Compute proportions
foaling_data$Total <- foaling_data$Foaling + foaling_data$Not_foaling
foaling_data$Foaling_prop <- foaling_data$Foaling / foaling_data$Total
foaling_data$Not_foaling_prop <- foaling_data$Not_foaling / foaling_data$Total

# Reshape data into long format for ggplot (necessary for stacked bars)
foaling_long <- foaling_data %>%
  tidyr::pivot_longer(cols = c(Foaling_prop, Not_foaling_prop), 
                      names_to = "Pregnancy_status", 
                      values_to = "percent")

# Rename levels for clarity
foaling_long$Pregnancy_status <- factor(foaling_long$Pregnancy_status, 
                                        levels = c("Not_foaling_prop", "Foaling_prop"),
                                        labels = c("Not foaling", "Foaling"))

# Define custom x-axis labels with sample sizes
custom_labels <- paste0(foaling_data$Year, "\n(", foaling_data$Total, ")")

# Plot
ggplot(foaling_long, aes(x = Year, y = percent, fill = Pregnancy_status)) +
  geom_col(position = "stack") +  # Stacked bars with calculated proportions
  scale_x_discrete(labels = custom_labels) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +  # Convert to % format
  labs(x = "\nYear (sample size)", 
       y = "% of all individuals with data for each year\n", 
       fill = "Status") +
  scale_fill_manual(values = c("#1C86EE", "#CD6600")) +  # Matching color scheme
  theme_classic()


# numbers of horse and cattle

letszam <- read_excel("data/letszam.xlsx")

letszam_long <- pivot_longer(letszam, cols = c("horses", "cattle"), names_to = "species", values_to = "count")
(letszam_plot <- ggplot(letszam_long, aes(x = year, y = count, color = species, group = species)) +
  geom_line() +
  geom_point() +
  labs(x = "\nYear", y = "Count\n") +
  theme_classic()+
  theme(legend.title = element_blank()))


letszam_long <- pivot_longer(letszam, cols = c("horses", "births", "deaths"), names_to = "data_type", values_to = "count")
(letszam_justhorses <- ggplot (letszam_long, aes (x = year, y = count, color = data_type, group = data_type))+
  geom_line()+
  geom_point()+
  labs (x = "\nYear", y = "Count\n")+
    theme_classic()+
    theme(legend.title = element_blank()))
  
letszam_long <- pivot_longer(letszam, cols = c("horses", "births"), names_to = "data_type", values_to = "count")

(letszam_justhorses <- ggplot (letszam_long, aes (x = year, y = count, color = data_type, group = data_type))+
  geom_line(size = 0.8)+
  geom_point(size = 2.5)+
  labs (x = "\nYear", y = "Number of animals\n")+
  theme_classic()+
    scale_color_manual(values = c("horses" = "#3B9AB2",   
                                  "births" = "#CD0000",   
                                  "deaths" = "#666666")) + 
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 12),
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 12)))
ggsave("pzp/plots/headcounts.png", dpi = 600)
c("#666666", "#EE7621", "#104E8B")
c("#FF7F00", "#EE2C2C", "#CD0000")


# years / animals treated

# age distribution ----


nrow(reversibility_3vacc)
reversibility_2vacc$age_primer_years <- (reversibility_2vacc$age_at_primer)/365
reversibility_3vacc$age_primer_years <- (reversibility_3vacc$age_at_primer)/365
mean(reversibility_2vacc$age_primer_years)
# 3.651
mean(reversibility_3vacc$age_primer_years)
# 6.47 !!!
(histogram_age_2vacc <- ggplot(reversibility_2vacc, aes (age_primer_years))+
  geom_histogram())
(histogram_age_3vacc <-ggplot(reversibility_3vacc, aes (age_primer_years))+
    geom_histogram())
