library(readxl)
library(tidyverse)

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