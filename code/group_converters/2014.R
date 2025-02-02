# group composition - 2014
# 2024-01-09

library(readxl)
library(tidyverse)
library(writexl)

groups <- read_excel("przewalski/data/groups/2014.xlsx", sheet = 15)

groups <- groups %>% 
  select(!(contains('...') | contains('bach')))

colnames(groups) <- as.character(unlist(groups[1,]))

groups <- subset(groups, select = -Bachelors)

groups_exp <- gather(groups, harem, name)
groups_exp <- drop_na(groups_exp)


# groups_exp <- groups %>% gather( harem, name) %>% 
 groups_exp <- groups_exp %>% 
  #drop_na() %>% 
  mutate(date =rep((as.Date("2014-11-26")),times=(nrow(groups_exp))),
         harem = stri_trans_general(str = groups_exp$harem, id = "Latin-ASCII"),
         name = stri_trans_general(str = groups_exp$name, id = "Latin-ASCII")) %>% 
  mutate(name = trimws(str_replace(name, "\\(.*?\\)", "")), 
           harem = trimws(str_replace(harem, "\\(.*?\\)", "")))

write_xlsx(groups_exp,"przewalski/data/groups/long/2013-11-26.xlsx") # this is an error!!

total <- rbind(total,groups_exp)

write_xlsx(total, "przewalski/data/groups/long/total_current.xlsx")
