# pzp analysis
# 22-01-2024
# packages and data ----
library(tidyverse)
library(readxl)
library(ggrepel)

pzp <- read_excel("przewalski/data/pzp/pzpdata.xlsx")
pzp <- mutate_if(pzp, is.POSIXct, as.Date)
# efficiency for horses who had both primer & booster on time, in first year. ----

horses_class1 <- pzp %>% 
  subset(Primer <= efficiency_primer & Booster1 <= efficiency_booster)  

nrow(horses_class1)/nrow(pzp)
# 45 % of horses got both primer & booster on time!

View(horses_class1)

# (timing == "p_b_yes" & is.na(Foal_DOB_P_1yr)) ~ "yes"

# if we only look at horses who we have data on in 1st year
# so remove OUT 

horses_class1 <- horses_class1 %>% 
  subset(Foal_Status_P_1yr != "OUT" &Foal_Status_P_1yr !="NK" &!is.na(Foal_Status_P_1yr))
nrow(horses_class1)
# sample size = 56
# for horses who got both P&B on time and we have data on pregnancy in 1st year after treatment
summary_class1<- horses_class1 %>% 
  group_by(Foal_Status_P_1yr) %>% 
  summarise(count = length(Name)) %>%
  ungroup()
summary_class1
48/(48+8) 
# 86% effective
8/(48+8)
# 14 % pregnant


# efficiency for horses who had P on time but booster late----
horses_class2 <- pzp %>% 
    subset(Primer <= efficiency_primer & Booster1 >efficiency_booster)
nrow(horses_class2)/nrow(pzp)
# 22% % of horses got both primer on time and booster late

View(horses_class2)

# remove those w no data

horses_class2 <- horses_class2 %>% 
  subset(Foal_Status_P_1yr != "OUT" &Foal_Status_P_1yr !="NK" &!is.na(Foal_Status_P_1yr))
nrow(horses_class2)
# sample size = 31
# for horses who got P time, B late and we have data on pregnancy in 1st year after treatment
summary_class2<- horses_class2 %>% 
  group_by(Foal_Status_P_1yr) %>% 
  summarise(count = length(Name)) %>%
  ungroup()
summary_class2
19/(19+12) 
# 61.3% effective
12/(19+12)
# 38.7% pregnant


# efficiency for horses who had P and B late----
horses_class3 <- pzp %>% 
  subset(Primer > efficiency_primer & Booster1 >efficiency_booster)
nrow(horses_class3)/nrow(pzp)
# 29.2 % of horses got p& b late

View(horses_class3)

# remove those w no data

horses_class3 <- horses_class3 %>% 
  subset(Foal_Status_P_1yr != "OUT" &Foal_Status_P_1yr !="NK" &!is.na(Foal_Status_P_1yr))
nrow(horses_class3)
# sample size = 34
# for horses who got both P&B late and we have data on pregnancy in 1st year after treatment
  summary_class3<- horses_class3 %>% 
  group_by(Foal_Status_P_1yr) %>% 
  summarise(count = length(Name)) %>%
  ungroup()
summary_class3
19/(19+15) 
# 56%% effective
11/(10+11)
# 52.4% pregnant


# using subset for whom exact date was calculated for breeding season ----

# delta for efficiency date to primer given, to booster, and for t between p & b

pzp <- pzp %>% 
  mutate(delta_to_primer = as.numeric(Primer - efficiency_primer),
         delta_to_booster = as.numeric(Booster1-efficiency_booster),
         primer_to_booster = as.numeric(Booster1-Primer))

# Divide the dataset into two – one includes individuals for which the exact date is calculated 
# and the other group has the average date based on the group estimate

# new subsets for this: 

exact <- subset(pzp, (!is.na(DOB_foal_treatment)) | (!is.na(Foal_DOB_P_1yr)))
nrow(exact) 
# 92 horses
# removing those with no data for the first year: 
exact <- subset(exact, Foal_Status_P_1yr != "OUT" &Foal_Status_P_1yr !="NK" &!is.na(Foal_Status_P_1yr))
nrow(exact) # 81 horses not bad
# horses who: got primer, and we have data on pregnant/not in 1st year.
exact$Foal_Status_P_1yr<- as.factor(exact$Foal_Status_P_1yr)
str(exact$Foal_Status_P_1yr) #NP or YES - good

estimate <- subset (pzp, is.na(DOB_foal_treatment)& is.na(Foal_DOB_P_1yr))
nrow(estimate) # 79 horses
estimate<- subset(estimate, Foal_Status_P_1yr != "OUT" &Foal_Status_P_1yr !="NK" &!is.na(Foal_Status_P_1yr))
nrow(estimate) # 42 horses

# plots 

(plot_primer <- ggplot(exact, aes(x = Foal_Status_P_1yr, y = delta_to_primer))+
    geom_point()+
    labs(x = "\nPregnant in year after primer given", y = "Primer early or late (days)\n")+
    scale_x_discrete(labels = c("not pregnant", "pregnant"))+
    theme_classic()+
    theme(axis.title.x = element_text (size=12),
          axis.text.x = element_text(size = 11),
          axis.title.y = element_text (size = 12),
          axis.text.y=element_text (size = 11)))
ggsave("przewalski/plots_pzp/plot_primer.png", width = 8, height = 5, dpi = 300)

(plot_booster <- ggplot(exact, aes(x = Foal_Status_P_1yr, y = delta_to_booster))+
    geom_point()+
    labs(x = "\nPregnant in year after primer given", y = "Booster early or late (days)\n")+
    scale_x_discrete(labels = c("not pregnant", "pregnant"))+
    geom_label_repel( 
      data = subset(exact, Name == "Rhea"),
      aes(x = (Foal_Status_P_1yr), y = delta_to_booster, label = "Rhea"))+
    theme_classic()+
    theme(axis.title.x = element_text (size=12),
          axis.text.x = element_text(size = 11),
          axis.title.y = element_text (size = 12),
          axis.text.y=element_text (size = 11)))
ggsave("przewalski/plots_pzp/plot_booster.png", width = 8, height = 5, dpi = 300)


outlier_booster <- pzp %>% filter(delta_to_booster >300)
# rhea, semiramis, rona

(plot_primer_to_booster <- ggplot(exact, aes(x = Foal_Status_P_1yr, y = primer_to_booster))+
    geom_point()+
    labs(x = "\nPregnant in year after PZP given", y = "Days between primer and booster\n")+
    scale_x_discrete(labels = c("not pregnant", "pregnant"))+
    theme_classic()+
    geom_label_repel( 
      data = subset(exact, Name == "Rhea"),
      aes(x = (Foal_Status_P_1yr), y = primer_to_booster, label = "Rhea"))+
    theme(axis.title.x = element_text (size=12),
          axis.text.x = element_text(size = 11),
          axis.title.y = element_text (size = 12),
          axis.text.y=element_text (size = 11)))
ggsave("przewalski/plots_pzp/plot_primertobooster.png", width = 8, height = 5, dpi = 300)


(plot_primer_foal<- ggplot (exact, aes(x = delta_to_primer, y = days_primer_to_firstfoal))+
  geom_point()+
    theme_classic())

# sample sizes ----
exact_booster <- subset(pzp, ((!is.na(DOB_foal_treatment)) | (!is.na(Foal_DOB_P_1yr))) & !is.na(Booster1))

nrow(exact_booster) # 90

values_only_in_exact <- anti_join(exact, exact_booster, by = "Name")
(values_only_in_exact) # almond and roxane didnt get booster1

exact_y1 <- subset(exact, (!is.na(Foal_Status_P_1yr) &  Foal_Status_P_1yr !="NK" & Foal_Status_P_1yr != "OUT"))
nrow(exact_y1) # 81

exact_y2 <- exact %>% filter( Foal_Status_P_2yr != "OUT" &Foal_Status_P_2yr !="NK" &!is.na(Foal_Status_P_2yr))
nrow(exact_y2) # 69

exact_y3 <- exact %>% filter( Foal_Status_P_3yr != "OUT" &Foal_Status_P_3yr !="NK" &!is.na(Foal_Status_P_3yr))
nrow(exact_y3) #54

exact_y4 <- exact %>% filter( Foal_Status_P_4yr != "OUT" &Foal_Status_P_4yr !="NK" &!is.na(Foal_Status_P_4yr))
nrow(exact_y4) # 42

exact_y5 <- exact %>% filter( Foal_Status_P_5yr != "OUT" &Foal_Status_P_5yr !="NK" &!is.na(Foal_Status_P_5yr))
nrow(exact_y5) #23

exact_y6 <- exact %>% filter( Foal_Status_P_6yr != "OUT" &Foal_Status_P_6yr !="NK" &!is.na(Foal_Status_P_6yr))
nrow(exact_y6) # 21

exact_y7 <- exact %>% filter( Foal_Status_P_7yr != "OUT" &Foal_Status_P_7yr !="NK" &!is.na(Foal_Status_P_7yr))
nrow(exact_y7) #18

# summaries for horses in exact category, regardless of timing ----
# year 1 ----
primer_only_y1 <- subset(exact,
                         is.na(Booster1))
nrow(primer_only_y1)
# roxane - primer only, almond - no data yet on efficiency
primer_booster_y1 <- subset(exact,
                            !is.na(Booster1)&
                            (!is.na(Foal_Status_P_1yr) & 
                             Foal_Status_P_1yr !="NK" & 
                             Foal_Status_P_1yr != "OUT"))
nrow(primer_booster_y1) # 80

summary_pby1<- primer_booster_y1 %>% 
  group_by(Foal_Status_P_1yr) %>% 
  summarise(count = length(Name)) %>%
  ungroup()
summary_pby1
45/80
# 56. 25 % effective
35/80
# 43.75 % pregnant

ontime_y1 <- primer_booster_y1 %>% 
  subset(Primer < efficiency_primer & Booster1 < efficiency_booster)
nrow(ontime_y1)
# sample size = 38
# for horses who got both P&B on time and we have data on pregnancy in 1st year after treatment
summary_y1_ontime<- ontime_y1 %>% 
  group_by(Foal_Status_P_1yr) %>% 
  summarise(count = length(Name)) %>%
  ungroup()
summary_y1_ontime
30/(38) 
# 79%% effective
8/(38)
# 21 % pregnant
p_yes_b_no_y1 <- primer_booster_y1 %>% 
  subset(Primer < efficiency_primer & Booster1 > efficiency_booster)
nrow(p_yes_b_no_y1) # 22

summary_y1_pyesbno<- p_yes_b_no_y1 %>% 
  group_by(Foal_Status_P_1yr) %>% 
  summarise(count = length(Name)) %>%
  ungroup()
summary_y1_pyesbno
10/(22) 
# 45% effective
12/(22)

p_b_no_y1 <- primer_booster_y1 %>% 
  subset(Primer > efficiency_primer & Booster1 > efficiency_booster)
nrow(p_b_no) # 19

summary_y1_pbno<- p_b_no %>% 
  group_by(Foal_Status_P_1yr) %>% 
  summarise(count = length(Name)) %>%
  ungroup()
summary_y1_pbno
4/(19) 
# 21 % effective
15/(19) 

df <- data.frame(
  timing_of_vaccines = c("a","a",
                         "b", "b",
                         "c","c" ),
  pregnant = c("pregnant", "not pregnant", "pregnant", "not pregnant","pregnant", "not pregnant"),
  proportion =c(21, 79, 55, 45, 79, 21)
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
           label = c("n = 38", "n = 22", "n = 19"), size = 4)

ggsave("przewalski/pzp/year1_barplot.png", width = 7, height = 5, dpi = 300)

# year 2 barplot -----
primer_booster_y2 <- subset(exact,
                            !is.na(Booster1)& !is.na(Booster2) &
                              (!is.na(Foal_Status_P_2yr) & 
                                 Foal_Status_P_2yr !="NK" & 
                                 Foal_Status_P_1yr != "OUT"&
                                 Foal_Status_P_2yr != "OUT"))
nrow(primer_booster_y2)
summary_y2<- primer_booster_y2 %>% 
  group_by(Foal_Status_P_2yr) %>% 
  summarise(count = length(Name)) %>%
  ungroup()
summary_y2
52/(11+52)

ontime_y2 <- primer_booster_y2 %>% 
  subset(Primer < efficiency_primer & Booster1 < efficiency_booster & !is.na(Booster2))
nrow(ontime_y2)
# sample size = 27
# for horses who got both P&B on time and we have data on pregnancy in 1st year after treatment
summary_y2_ontime<- ontime_y2 %>% 
  group_by(Foal_Status_P_2yr) %>% 
  summarise(count = length(Name)) %>%
  ungroup()
summary_y2_ontime
23/(27) 
# 85.2% effective
4/(27)
# 14.8 % pregnant
p_yes_b_no_y2 <- primer_booster_y2 %>% 
  subset(Primer < efficiency_primer & Booster1 > efficiency_booster& !is.na(Booster2))
nrow(p_yes_b_no_y2) # 18

summary_y2_pyesbno<- p_yes_b_no_y2 %>% 
  group_by(Foal_Status_P_2yr) %>% 
  summarise(count = length(Name)) %>%
  ungroup()
summary_y2_pyesbno
17/(18) 
# 94% effective
1/(18)
# 6 % pregnant

p_b_no_y2 <- primer_booster_y2 %>% 
  subset(Primer > efficiency_primer & Booster1 > efficiency_booster & !is.na(Booster2))
nrow(p_b_no_y2) # 18

summary_y2_pbno<- p_b_no_y2 %>% 
  group_by(Foal_Status_P_2yr) %>% 
  summarise(count = length(Name)) %>%
  ungroup()
summary_y2_pbno
12/(18) 
# 66.7 % effective
6/(18) 
# 33.3 % effective

# y2_b2_no
y2_b2_no <- subset(exact,
                   !is.na(Booster1)& is.na(Booster2) &
                     (!is.na(Foal_Status_P_2yr) & 
                        Foal_Status_P_2yr !="NK" & 
                        Foal_Status_P_1yr != "OUT"&
                        Foal_Status_P_2yr != "OUT"))
nrow(y2_b2_no) # 6 horses 
sum_y2_b2_no<- y2_b2_no %>% 
  group_by(Foal_Status_P_2yr) %>% 
  summarise(count = length(Name)) %>%
  ungroup()
sum_y2_b2_no
# 2 pregnant, 4 not pregnant

df2 <- data.frame(
  timing_of_vaccines = c("a","a",
                         "b", "b",
                         "c","c" ),
  pregnant = c("pregnant", "not pregnant", "pregnant", "not pregnant","pregnant", "not pregnant"),
  proportion =c(14.8, 85.2, 6, 94, 33.3, 66.7)
)

(year2_barplot <- ggplot(df2, aes(x = timing_of_vaccines, y = proportion, fill = factor(pregnant))) +
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
label_y_position <- max(df2$proportion) + 5

year2_barplot +
  annotate("text", x = c(1, 2, 3), y = rep(label_y_position, 3),
           label = c("n = 27", "n = 18", "n = 18"), size = 4)

ggsave("przewalski/pzp/year2_barplot.png", width = 7, height = 5, dpi = 300)

# y3 ----
# subset of whom we have data for year 3
primer_booster_y3 <- subset(exact,
                            !is.na(Booster1)&
                              (!is.na(Foal_Status_P_3yr) & 
                                 Foal_Status_P_3yr !="NK" & 
                                 Foal_Status_P_3yr != "OUT"&
                                 Foal_Status_P_3yr != "OUT"))
nrow(primer_booster_y3) # 54

# group 1 - got B3 & B2
booster3 <- primer_booster_y3 %>%
  subset(!is.na(Booster3) & !is.na(Booster2))
nrow(booster3) # only 16 horses got B3

summary_b3 <- booster3 %>% 
  group_by(Foal_Status_P_3yr) %>% 
  summarise(count = length(Name)) %>% 
  ungroup()
summary_b3
14/16
b3_pregnant <- subset(booster3, Foal_Status_P_3yr == "YES")
b3_pregnant

# group 2 - no B3 but y B2
no_b3 <- primer_booster_y3 %>% 
  subset (is.na(Booster3)& !is.na(Booster2))
nrow(no_b3) # 34
# efficiency overall
summary_nob3 <- no_b3 %>% 
  group_by(Foal_Status_P_3yr) %>% 
  summarise(count = length(Name)) %>% 
  ungroup()
summary_nob3
23/(23+11)

# divide this group based on initial timings of vaccines
ontime_y3 <- no_b3 %>% 
  subset(Primer < efficiency_primer & Booster1 < efficiency_booster )
nrow(ontime_y3) 
# sample size = 19
# for horses who got both P&B on time and we have data on pregnancy in 3rd year after treatment
summary_y3_ontime<- ontime_y3 %>% 
  group_by(Foal_Status_P_3yr) %>% 
  summarise(count = length(Name)) %>%
  ungroup()
summary_y3_ontime
13/(19) 
# 68.4% effective
6/(19)
# 31.6 % pregnant

p_yes_b_no_y3 <- no_b3 %>% 
  subset(Primer < efficiency_primer & Booster1 > efficiency_booster )
nrow(p_yes_b_no_y3) # 5 only

summary_y3_pyesbno<- p_yes_b_no_y3 %>% 
  group_by(Foal_Status_P_3yr) %>% 
  summarise(count = length(Name)) %>%
  ungroup()
summary_y3_pyesbno # 5 horses only
# 4 np, 1 pregnant (80% efficiency)

p_b_no_y3 <- no_b3 %>% 
  subset(Primer > efficiency_primer & Booster1 > efficiency_booster )
nrow(p_b_no_y3) # 10

summary_y3_pbno<- p_b_no_y3 %>% 
  group_by(Foal_Status_P_3yr) %>% 
  summarise(count = length(Name)) %>%
  ungroup()
summary_y3_pbno # 5 horses only
# 6 np, 4 pregnant (60% efficiency)


# y3_b2_no
y3_b2_no <- primer_booster_y3 %>% 
  subset(is.na(Booster2))
nrow(y3_b2_no) # 4 horses 
sum_y3_b2_no<- y2_b2_no %>% 
  group_by(Foal_Status_P_3yr) %>% 
  summarise(count = length(Name)) %>%
  ungroup()
sum_y3_b2_no
# 3 pregnant, 1 not pregnant

df2 <- data.frame(
  timing_of_vaccines = c("a","a",
                         "b", "b",
                         "c","c" ),
  pregnant = c("pregnant", "not pregnant", "pregnant", "not pregnant","pregnant", "not pregnant"),
  proportion =c(14.8, 85.2, 6, 94, 33.3, 66.7)
)

(year2_barplot <- ggplot(df2, aes(x = timing_of_vaccines, y = proportion, fill = factor(pregnant))) +
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
label_y_position <- max(df2$proportion) + 5

year2_barplot +
  annotate("text", x = c(1, 2, 3), y = rep(label_y_position, 3),
           label = c("n = 27", "n = 18", "n = 18"), size = 4)

ggsave("przewalski/pzp/year2_barplot.png", width = 7, height = 5, dpi = 300)

# y4 - 23rd may----
primer_booster_y4 <- subset(exact,
                            !is.na(Booster1)&
                              (!is.na(Foal_Status_P_4yr) & 
                                 Foal_Status_P_4yr !="NK" & 
                                 Foal_Status_P_4yr != "OUT")&
                              # not treated again
                             (! (!is.na(Primer_again) & Primer_again < Primer + 1460) )&
                              (! (!is.na(Booster1_again) & Booster2_again < Primer + 1460) )& 
                              (! (!is.na(Booster2_again) & Booster2_again < Primer + 1460) ) 
)
nrow(primer_booster_y4) # 34


# group 2 - no B3 and B4 but y B2
no_b3_y4 <- primer_booster_y4 %>% 
  subset (is.na(Booster3)& (is.na(Booster4)) & !is.na(Booster2))
nrow(no_b3_y4) # 19

sum_y4<- no_b3_y4 %>% 
  group_by(Foal_Status_P_4yr) %>% 
  summarise(count = length(Name)) %>%
  ungroup()
sum_y4
18/19

# year 5 - 23rd may----
primer_booster_y5 <- subset(exact,
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
nrow(no_b3_y5) # 10

sum_y5<- no_b3_y5 %>% 
  group_by(Foal_Status_P_5yr) %>% 
  summarise(count = length(Name)) %>%
  ungroup()
sum_y5

# year 6 - 23rd may----
primer_booster_y6 <- subset(exact,
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
nrow(primer_booster_y6) # 20


# group 2 - no B3 and B4 but y B2
no_b3_y6 <- primer_booster_y6 %>% 
  subset (is.na(Booster3)& (is.na(Booster4)) & !is.na(Booster2))
nrow(no_b3_y6) # 10

sum_y6<- no_b3_y6 %>% 
  group_by(Foal_Status_P_6yr) %>% 
  summarise(count = length(Name)) %>%
  ungroup()
sum_y6

# year 7 - 23rd may----
primer_booster_y7 <- subset(exact,
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
nrow(primer_booster_y7) # 12


# group 2 - no B3 and B4 but y B2
no_b3_y7 <- primer_booster_y7 %>% 
  subset (is.na(Booster3)& (is.na(Booster4)) & !is.na(Booster2))
nrow(no_b3_y7) # 4

sum_y7<- no_b3_y7 %>% 
  group_by(Foal_Status_P_7yr) %>% 
  summarise(count = length(Name)) %>%
  ungroup()
sum_y7

# plots - scatter ----
(plot_pby1 <- ggplot(primer_booster_y1, aes(x = Foal_Status_P_1yr, y = delta_to_primer))+
    geom_point()+
   geom_point(aes(colour= as.factor(timing)))+
   scale_color_manual(values = c("#BF3EFF", "#4ecb50", "#FF8C00"))+
   labs(color = "Timing of PZP\n") +
    labs(x = "\nPregnant in year after primer given", y = "Primer early or late (days)\n")+
    scale_x_discrete(labels = c("not pregnant", "pregnant"))+
    theme_classic()+
    theme(axis.title.x = element_text (size=12),
          axis.text.x = element_text(size = 11),
          axis.title.y = element_text (size = 12),
          axis.text.y=element_text (size = 11)))

(plot_pby1_2 <- ggplot(primer_booster_y1, aes(x = Foal_Status_P_1yr, y = delta_to_booster))+
    geom_point()+
    geom_point(aes(colour= as.factor(timing)))+
    scale_color_manual(values = c("#BF3EFF", "#4ecb50", "#FF8C00"))+
    labs(color = "Timing of PZP\n") +
    labs(x = "\nPregnant in year after primer given", y = "Booster early or late (days)\n")+
    scale_x_discrete(labels = c("not pregnant", "pregnant"))+
    geom_label_repel( 
      data = subset(exact, Name == "Rhea"),
      aes(x = (Foal_Status_P_1yr), y = delta_to_booster, label = "Rhea"))+
    theme_classic()+
    theme(axis.title.x = element_text (size=12),
          axis.text.x = element_text(size = 11),
          axis.title.y = element_text (size = 12),
          axis.text.y=element_text (size = 11)))

(plot_pby1_3 <- ggplot(primer_booster_y1, aes(x = Foal_Status_P_1yr, y = primer_to_booster))+
    geom_point()+
    geom_point(aes(colour= as.factor(timing)))+
    scale_color_manual(values = c("#BF3EFF", "#4ecb50", "#FF8C00"))+
    labs(color = "Timing of PZP\n") +
    labs(x = "\nPregnant in year after PZP given", y = "Days between primer and booster\n")+
    scale_x_discrete(labels = c("not pregnant", "pregnant"))+
    theme_classic()+
    geom_label_repel( 
      data = subset(exact, Name == "Rhea"),
      aes(x = (Foal_Status_P_1yr), y = primer_to_booster, label = "Rhea"))+
    theme(axis.title.x = element_text (size=12),
          axis.text.x = element_text(size = 11),
          axis.title.y = element_text (size = 12),
          axis.text.y=element_text (size = 11)))

# year 2 -----
primer_booster_y2 <- subset(exact,
                            !is.na(Booster1)&
                              (!is.na(Foal_Status_P_2yr) &
                                 Foal_Status_P_2yr !="NK" & 
                                 Foal_Status_P_2yr != "OUT"))

summary_pby2<- primer_booster_y2 %>% 
  group_by(Foal_Status_P_2yr) %>% 
  summarise(count = length(Name)) %>%
  ungroup()
summary_pby2
56/(56+13) # 81% effective

ontime_y2 <- primer_booster_y2 %>% 
  subset(Primer < efficiency_primer & Booster1 < efficiency_booster)
nrow(ontime_y2) # 29
# sample size = 56
# for horses who got both P&B on time and we have data on pregnancy in 1st year after treatment
summary_ontime_y2<- ontime_y2 %>% 
  group_by(Foal_Status_P_2yr) %>% 
  summarise(count = length(Name)) %>%
  ungroup()
summary_ontime_y2
24/(29) 
# 82% effective
5/(29)
# 17 % pregnant

# plots ----
(plot_pby2 <- ggplot(primer_booster_y2, aes(x = Foal_Status_P_2yr, y = delta_to_primer))+
   geom_point(aes(colour= as.factor(timing)))+
   scale_color_manual(values = c("#BF3EFF", "#4ecb50", "#FF8C00"))+
                        labs(color = "Timing of PZP\n") +
   labs(x = "\nPregnant in year 2 after primer given", y = "Primer early or late (days)\n")+
   scale_x_discrete(labels = c("not pregnant", "pregnant"))+
   theme_classic()+
   theme(axis.title.x = element_text (size=12),
         axis.text.x = element_text(size = 11),
         axis.title.y = element_text (size = 12),
         axis.text.y=element_text (size = 11)))

(plot_pby2_2 <- ggplot(primer_booster_y2, aes(x = Foal_Status_P_2yr, y = delta_to_booster))+
    geom_point()+
    geom_point(aes(colour= as.factor(timing)))+
    scale_color_manual(values = c("#BF3EFF", "#4ecb50", "#FF8C00"))+
    labs(color = "Timing of PZP\n") +
    labs(x = "\nPregnant in year 2 after primer given", y = "Booster early or late (days)\n")+
    scale_x_discrete(labels = c("not pregnant", "pregnant"))+
   # geom_label_repel( 
     # data = subset(exact, Name == "Rhea"),
    #  aes(x = (Foal_Status_P_2yr), y = delta_to_booster, label = "Rhea"))+
    theme_classic()+
    theme(axis.title.x = element_text (size=12),
          axis.text.x = element_text(size = 11),
          axis.title.y = element_text (size = 12),
          axis.text.y=element_text (size = 11)))

(plot_pby2_3 <- ggplot(primer_booster_y2, aes(x = Foal_Status_P_2yr, y = primer_to_booster))+
    geom_point()+
    geom_point(aes(colour= as.factor(timing)))+
    scale_color_manual(values = c("#BF3EFF", "#4ecb50", "#FF8C00"))+
    labs(color = "Timing of PZP\n") +
    labs(x = "\nPregnant in year 2 after PZP given", y = "Days between primer and booster\n")+
    scale_x_discrete(labels = c("not pregnant", "pregnant"))+
    theme_classic()+
   #  geom_label_repel( 
    #  data = subset(exact, Name == "Rhea"),
      # aes(x = (Foal_Status_P_2yr), y = primer_to_booster, label = "Rhea"))+
    theme(axis.title.x = element_text (size=12),
          axis.text.x = element_text(size = 11),
          axis.title.y = element_text (size = 12),
          axis.text.y=element_text (size = 11)))

# year 3 ----
primer_booster_y3 <- subset(exact,
                            !is.na(Booster1)&
                              (!is.na(Foal_Status_P_3yr) & 
                                 Foal_Status_P_3yr !="NK" & 
                                 Foal_Status_P_3yr != "OUT"))

summary_pby3<- primer_booster_y3 %>% 
  group_by(Foal_Status_P_3yr) %>% 
  summarise(count = length(Name)) %>%
  ungroup()
summary_pby3
38/(38+15) # 72%

ontime_y3 <- primer_booster_y3 %>% 
  subset(Primer < efficiency_primer & Booster1 < efficiency_booster)
nrow(ontime_y3)
# sample size = 25
# for horses who got both P&B on time and we have data on pregnancy in 1st year after treatment
summary_omtime_y3<- ontime_y3 %>% 
  group_by(Foal_Status_P_3yr) %>% 
  summarise(count = length(Name)) %>%
  ungroup()
summary_omtime_y3
15/(15+10) 
# 60% effective
10/(15+10)
# 40 % pregnant

(plot_pby3 <- ggplot(primer_booster_y3, aes(x = Foal_Status_P_3yr, y = delta_to_primer))+
    geom_point()+
    geom_point(aes(colour= as.factor(timing)))+
    scale_color_manual(values = c("#BF3EFF", "#4ecb50", "#FF8C00"))+
    labs(color = "Timing of PZP\n") +
    labs(x = "\nPregnant in year 3 after primer given", y = "Primer early or late (days)\n")+
    scale_x_discrete(labels = c("not pregnant", "pregnant"))+
    theme_classic()+
    theme(axis.title.x = element_text (size=12),
          axis.text.x = element_text(size = 11),
          axis.title.y = element_text (size = 12),
          axis.text.y=element_text (size = 11)))

(plot_pby3_2 <- ggplot(primer_booster_y3, aes(x = Foal_Status_P_3yr, y = delta_to_booster))+
    geom_point()+
    geom_point(aes(colour= as.factor(timing)))+
    scale_color_manual(values = c("#BF3EFF", "#4ecb50", "#FF8C00"))+
    labs(color = "Timing of PZP\n") +
    labs(x = "\nPregnant in year 3 after primer given", y = "Booster early or late (days)\n")+
    scale_x_discrete(labels = c("not pregnant", "pregnant"))+
    # geom_label_repel( 
    # data = subset(exact, Name == "Rhea"),
    #  aes(x = (Foal_Status_P_2yr), y = delta_to_booster, label = "Rhea"))+
    theme_classic()+
    theme(axis.title.x = element_text (size=12),
          axis.text.x = element_text(size = 11),
          axis.title.y = element_text (size = 12),
          axis.text.y=element_text (size = 11)))

(plot_pby3_3 <- ggplot(primer_booster_y3, aes(x = Foal_Status_P_3yr, y = primer_to_booster))+
    geom_point()+
    geom_point(aes(colour= as.factor(timing)))+
    scale_y_continuous(limits = c(0,700))+
    scale_color_manual(values = c("#BF3EFF", "#4ecb50", "#FF8C00"))+
    labs(color = "Timing of PZP\n") +
    labs(x = "\nPregnant in year 3 after PZP given", y = "Days between primer and booster\n")+
    scale_x_discrete(labels = c("not pregnant", "pregnant"))+
    theme_classic()+
    #  geom_label_repel( 
    #  data = subset(exact, Name == "Rhea"),
    # aes(x = (Foal_Status_P_2yr), y = primer_to_booster, label = "Rhea"))+
    theme(axis.title.x = element_text (size=12),
          axis.text.x = element_text(size = 11),
          axis.title.y = element_text (size = 12),
          axis.text.y=element_text (size = 11)))


