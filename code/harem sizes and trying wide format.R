library(readxl)
library(tidyverse)

library(readxl)

# importing big list
 nagylista <- read_excel("przewalski/data/nagylista.xls", 
                               col_types = c("text", "text", "text", 
                                                     "text", "date", "text", "text", "text", 
                                                      "text", "text", "text", "text", "date", 
                                                      "text", "date", "text", "text", "text", 
                                                    "text", "text", "text", "text", "text"))
 
 total_2<- read_excel( "przewalski/data/groups/long/total_current_2023.xlsx")
 unique(total_2$harem)
 
 
 # some dates not in the right format in nagylista- check
 
 nagylista$name<- as.character(nagylista$Name)
 
# merging the 2 datasets
 
 merged <- dplyr::left_join(total, nagylista, by = c("name"))
 
# new column for age
 merged<- merged %>% mutate (age = as.numeric(date - Date_of_birth))
str(merged)
merged$date<-as.Date(merged$date)
merged<- merged %>% mutate_if(is.POSIXct, as.Date)

# trying to aggregate observations into year
merged<- merged %>% mutate(year = format (as.Date (date, format = "%Y-%m-%d"), "%Y")) %>% 
  mutate_at("year", as.numeric) 

# Group by year, horse_name, group_name, and count occurrences
group_counts <- merged %>%
  group_by(year, name, harem) %>%
  summarise(count = n()) %>%
  ungroup()

# Find the group with maximum count for each year and horse
majority_group <- group_counts %>%
  group_by(year, name) %>%
  filter(count == max(count)) %>%
  select(-count)  # Remove the count column

# You can remove duplicates if you want to ensure there's only one observation per horse per year
# unique_majority_group <- distinct(majority_group)


result <- dplyr::left_join(majority_group, nagylista, by = c("name"))

# Y A Y 


total<- read_excel( "przewalski/data/groups/long/total_current_2023.xlsx")
total <- tibble::rowid_to_column(total, "ID")

total <- total %>%
  group_by(name, date, harem) %>%
  mutate(unique_id = row_number())

total_wide <- total %>%
  pivot_wider(names_from = date, values_from = harem, values_fill = NA)




haremsize<- total %>% 
  group_by(date, harem) %>% 
  summarise(harem_size = length(name))

haremsize$date <- as.Date(haremsize$date)

haremsize <- haremsize %>% 
  group_by(harem) %>% 
  mutate(harem_start = as.Date(min(date)),
         harem_experience = as.numeric(date - harem_start))

filtered<- haremsize %>% 
  filter(!grepl('UK', harem)) %>% 
  filter(!grepl('unk', harem)) %>% 
  filter(!grepl('uk', harem))
View(filtered)
         
harem_leander <- subset(haremsize, harem =="Leander")  
(plot_leander <- ggplot(harem_leander, aes(x = harem_experience, y = harem_size))+
  geom_point()+
  theme_bw())
# should maybe do averages for year.
(plot_haremsize <- ggplot(filtered, aes(x = harem_experience, y = harem_size,fill=harem ))+
    geom_point()+
    theme(legend.position = "none",
          legend.text = element_blank())+
    theme_bw())

model<- lm( harem_size ~ harem_experience, data = filtered)
summary(model)

haremsize_Leander <- subset(haremsize, harem == "Leander")

# issues:
# need to remove foals and bachelors.
# 