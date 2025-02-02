# 2024 - 02 - 08
# merging 2 datasets together and aggregating group affilations to years

library(readxl)
library(writexl)
library(tidyverse)

# import datasets
total<- read_excel( "przewalski/data/groups/long/total_hst_clean.xlsx")
nagylista <- read_excel("przewalski/data/nagylista.xls", 
                        col_types = c("text", "text", "text", 
                                      "text", "date", "text", "text", "text", 
                                      "text", "text", "text", "text", "date", 
                                      "text", "date", "text", "text", "text", 
                                      "text", "text", "text", "text", "text"))

# some dates not in the right format in nagylista - check

# merging

nagylista$name<- as.character(nagylista$Name)
merged <- dplyr::left_join(total, nagylista, by = c("name"))

merged$date<-as.Date(merged$date) # check if i need this
merged<- merged %>% mutate_if(is.POSIXct, as.Date)

merged<- merged %>% mutate (age = as.numeric(date - Date_of_birth)) 
# here is your chance to remove foals etc

# save this dataset
write_xlsx(merged, "przewalski/data/complete_merged.xlsx")

# aggregating observations by year

merged<- merged %>% 
  mutate(year = format (as.Date (date, format = "%Y-%m-%d"), "%Y")) %>% 
  mutate_at("year", as.numeric)

# Group by year, name, harem, and count occurrences
group_counts <- merged %>%
  group_by(year, name, harem) %>%
  summarise(count = n()) %>%
  ungroup()

# Find the group with maximum count for each year and horse
majority_group <- group_counts %>%
  group_by(year, name) %>%
  filter(count == max(count)) 
 # select(-count) 

View(majority_group)

# a horse is found in different harems for equal number of observations - 
# what do I keep?
# maybe the last observation, as that is the one most likely to document a harem change.
# also issue with exact age - if the date of the observation is not exact, i can't know
# the exact age of the horse.

# add the individual characteristics of horses
years_full <- dplyr::left_join(majority_group, nagylista, by = c("name"))

write_xlsx(years_full, "przewalski/data/years_groups_complete.xlsx")

years_full <- years_full %>%
  arrange(year) %>%
  group_by(name) %>%
  mutate(previous_harem = lag(harem)) %>%
  ungroup()

years_full <- years_full %>% 
  group_by (year, name) %>% 
  mutate(change = case_when(
    harem == previous_harem ~ 0,
    harem != previous_harem ~ 1,
    is.na(previous_harem) ~ NA)) %>% 
  select(-Note) %>% 
  ungroup()

# how many harems per year for females? 
# how often do they change harems multiple times a year?

# summarise by year, count number of unique harem observations
merged$harem<- as.factor(merged$harem)
harem_counts <- merged %>% 
  group_by(year, name) %>% 
  summarise (count = length(unique(harem))) %>% 
  ungroup()
View(harem_counts)
(histogram_haremcounts <- ggplot(harem_counts, aes(x = count))+
    geom_histogram()+
    theme_bw())
harem_counts$count<- as.factor(harem_counts$count)
summary_haremcounts <- harem_counts %>% 
  group_by(count) %>% 
  summarise (occurrence = length(count)) %>% 
  ungroup()
View(summary_haremcounts)

# quite a few still that occur in 2 harems in a year
# would be nice to document
# but if they stay in the 2nd harem for the next year its not actually losing data?
# its only when its 3 or more when some data is lost. (which is ok i guess)

# change or not factor.
# if occur in year and year+1
# change/not
# if doesnt occur in previous year: NA
str(years_full)
years_full<- years_full %>% 
  mutate(year_before = year-1) %>% 
  group_by(year, name) %>% 
    previous_harem = )





    
# how to distinguish when its a harem stallion change and not a single female leaving the group??

