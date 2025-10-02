# starts with group who got 3 vaccines, based on code to get efficacy for b1group.

nrow(reversibility_3vacc)
View(reversibility_3vacc)
# booster4 and 3 already excluded.
# maybe remove horse that got B2 late?

# making the yearly groups
y1_2b <- reversibility_3vacc %>% filter( Foal_Status_P_1yr != "OUT" &Foal_Status_P_1yr !="NK" &!is.na(Foal_Status_P_1yr))
nrow(y1_2b) # 77

y2_2b <- reversibility_3vacc %>% filter( Foal_Status_P_2yr != "OUT" &Foal_Status_P_2yr !="NK" &!is.na(Foal_Status_P_2yr))
nrow(y2_2b) #66 

y3_2b <- reversibility_3vacc %>% filter( Foal_Status_P_3yr != "OUT" &Foal_Status_P_3yr !="NK" &!is.na(Foal_Status_P_3yr))
nrow(y3_2b) # 51
y3_2b<- y3_2b %>% filter(!(Name == "Tekla"))

y4_2b <- reversibility_3vacc %>% filter( Foal_Status_P_4yr != "OUT" &Foal_Status_P_4yr !="NK" &!is.na(Foal_Status_P_4yr))
nrow(y4_2b) # 35
y4_2b<- y4_2b %>% 
  filter(!(Name== "Rozmaring" ))


y5_2b <- reversibility_3vacc %>% filter( Foal_Status_P_5yr != "OUT" &Foal_Status_P_5yr !="NK" &!is.na(Foal_Status_P_5yr))
nrow(y5_2b) # 17
y5_2b <- y5_2b %>% 
  filter(!(Name == "Selymes"))

y6_2b <- reversibility_3vacc %>% filter( Foal_Status_P_6yr != "OUT" &Foal_Status_P_6yr !="NK" &!is.na(Foal_Status_P_6yr))
nrow(y6_2b) # 12

y7_2b <- reversibility_3vacc %>% filter( Foal_Status_P_7yr != "OUT" &Foal_Status_P_7yr !="NK" &!is.na(Foal_Status_P_7yr))
nrow(y7_2b) # 9

y8_2b <- reversibility_3vacc %>% filter( Foal_Status_P_8yr != "OUT" &Foal_Status_P_8yr !="NK" &!is.na(Foal_Status_P_8yr))
nrow(y8_2b) # 4
y8_2b <-y8_2b %>% 
  filter(!(Name == "Orchidea" )) %>% 
  filter(!(Name == "Nina"))

y9_2b <- reversibility_3vacc %>% filter( Foal_Status_P_9yr != "OUT" &Foal_Status_P_9yr !="NK" &!is.na(Foal_Status_P_9yr))
nrow(y9_2b) # 3

y10_2b <- reversibility_3vacc %>% filter( Foal_Status_P_10yr != "OUT" &Foal_Status_P_10yr !="NK" &!is.na(Foal_Status_P_10yr))
nrow(y10_2b) # 0 ok

# year by year----
# y1 -----
summary_y1<- y1_2b %>% 
  group_by(Foal_Status_P_1yr) %>% 
  summarise(count = length(Name)) %>%
  ungroup()
summary_y1
# 58, 19, 75.3%, n = 77

# y2 -----
summary_y2<- y2_2b %>% 
  group_by(Foal_Status_P_2yr) %>% 
  summarise(count = length(Name)) %>%
  ungroup()
summary_y2
58/(58+8) # 87.8 % effective
# 66 = n

# y3-----
summary_y3<- y3_2b %>% 
  group_by(Foal_Status_P_3yr) %>% 
  summarise(count = length(Name)) %>%
  ungroup()
summary_y3
40/(40+11) # 78.4%,  n = 51

# y4 -----
summary_y4<- y4_2b %>% 
  group_by(Foal_Status_P_4yr) %>% 
  summarise(count = length(Name)) %>%
  ungroup()
summary_y4
31/(31+4) # ss 35
# 88.57%

# y5 -----
summary_y5<- y5_2b %>% 
  group_by(Foal_Status_P_5yr) %>% 
  summarise(count = length(Name)) %>%
  ungroup()
summary_y5
12/17 # 70.5%, n= 17

# y6-----
summary_y6<- y6_2b %>% 
  group_by(Foal_Status_P_6yr) %>% 
  summarise(count = length(Name)) %>%
  ungroup()
summary_y6
7/12 # 58.3%, n= 12

# y7----
summary_y7<- y7_2b %>% 
  group_by(Foal_Status_P_7yr) %>% 
  summarise(count = length(Name)) %>%
  ungroup()
summary_y7
8/9 # 88.9%, n= 9

# y8----

summary_y8<- y8_2b %>% 
  group_by(Foal_Status_P_8yr) %>% 
  summarise(count = length(Name)) %>%
  ungroup()
summary_y8
3/4 # 75%, n= 4

# y9-----
summary_y9<- y9_2b %>% 
  group_by(Foal_Status_P_9yr) %>% 
  summarise(count = length(Name)) %>%
  ungroup()
summary_y9
# 3 np, 100%

# graph ----
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

ggsave("pzp/plots/yearly_efficacy_2boosters_new_9years.png", dpi = 600)

str(reversibility_3vacc)
reversibility_3vacc$age_at_primer = as.numeric(((reversibility_3vacc$Primer-reversibility_3vacc$birth_date)/365))
mean(reversibility_3vacc$age_at_primer)
(histogram_age_3vacc <-ggplot(reversibility_3vacc, aes (age_at_primer))+
    geom_histogram()+
    theme_classic())


write_xlsx(reversibility_3vacc, "data/pzp/reversibility_3vacc.xlsx")
