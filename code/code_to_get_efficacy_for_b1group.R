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

# got an initial primer and booster
pandb <- pzp %>%  filter(!is.na(Primer)& !is.na(Booster1))

str(pandb [, 30:39])


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
nrow(reversibility_3vacc)

# 2 vaccines, summary


reversibility_2vacc <- reversibility_2vacc %>% 
  mutate (years_between_primer_and_p2 = ifelse(is.na(Primer_again), NA, 
                                               (as.numeric(format(Primer_again, "%Y"))) - (as.numeric(format(Primer, "%Y"))))
  )

reversibility_2vacc <- reversibility_2vacc[reversibility_2vacc$Name != "Rhea", ]

reversibility_longer_2vacc <- reversibility_2vacc %>% 
  pivot_longer( cols= c(25:34), names_to = "Year_of_foal", values_to = "Pregnancy_status" )
View(reversibility_longer_2vacc)
# smth not ok with this, but i counted by hand lol. And no one needs to be removed here based on new Primer.

reversibility_longer_percent_2vacc <- reversibility_longer_2vacc %>%
  group_by(Year_of_foal) %>%
  mutate(total = n()) %>%
  group_by(Year_of_foal, Pregnancy_status) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(percent = count / sum(count) * 100)  # Compute percentages
reversibility_longer_percent_2vacc<- reversibility_longer_percent_2vacc %>% 
  filter(!(Pregnancy_status== "NK") ) %>% 
  filter(!is.na(Pregnancy_status)) %>% 
  filter(!(Pregnancy_status== "OUT" ))
View(reversibility_longer_percent_2vacc)
# Plot as 100% stacked bars
(plot_by_year_2vacc <- ggplot(reversibility_longer_percent_2vacc, 
                              aes(x = Year_of_foal, 
                                  y = percent, 
                                  fill = Pregnancy_status)) +
    scale_x_discrete(labels = c("1\n (25)", "2\n(17)", "3\n(14)", 
                                "4\n (11)", "5\n (7)", "6\n (8)", 
                                "7\n (5)", "8\n(1)", "9\n(1)"))+
    geom_col(position = "dodge") +  # Fill makes bars 100% stacked
    labs(x = "\nYear (sample size)", y = "% of all individuals with data for each year\n", fill = "Status")+
    scale_fill_manual(values = c("orange", "skyblue"), labels = c("Not foaling", "Foaling")) +
    # scale_y_continuous(labels = scales::percent_format(scale = 1)) +  # Convert to percentage format
    theme_classic()+
    theme(legend.title = element_blank()))

ggsave("pzp/plots/returns_in_each_year_2vacc.png", dpi = 600)

# actual timeline plot ----
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
  labs (x = "\nYears after treatment started (Sample size)", y = "Proportion of mares\n" )+
  theme_classic()+
  theme(legend.title = element_blank(),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 12), 
        legend.text = element_text(size = 12))
c("#4F4F4F", "#474747", "#7A7A7A")

print(timeline_graph_1b)

ggsave("pzp/plots/yearly_efficacy_1booster_revised.png", dpi = 600)

str(reversibility_2vacc)
reversibility_2vacc$age_at_primer = as.numeric(((reversibility_2vacc$Primer-reversibility_2vacc$birth_date)/365))
mean(reversibility_2vacc$age_at_primer)
(histogram_age_2vacc <-ggplot(reversibility_2vacc, aes (age_at_primer))+
    geom_histogram()+
    theme_classic())
