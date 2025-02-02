# group composition - 2012/13
# 2024-01-03

library(readxl)
library(tidyverse)
library(writexl)

groups <- read_excel("przewalski/data/groups/2012.xlsx", sheet = 13)

groups <- groups %>% 
  select(!(contains('...') | contains('bach')))

colnames(groups) <- as.character(unlist(groups[1,]))

groups <- subset(groups, select = -Bachelors)

groups_exp <- gather(groups, harem, name)
groups_exp <- drop_na(groups_exp)
groups_exp$date<-rep((as.Date("2012-05-28")),times=(nrow(groups_exp)))

groups_exp$harem = stri_trans_general(str = groups_exp$harem, id = "Latin-ASCII")
groups_exp$name = stri_trans_general(str = groups_exp$name, id = "Latin-ASCII")

write_xlsx(groups_exp,"przewalski/data/groups/long/2012-05-28.xlsx")

total <- rbind(total,groups_exp)

write_xlsx(total, "przewalski/data/groups/long/total_current.xlsx")
