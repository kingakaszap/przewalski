# group composition - 2011/09/21
# 2023-12-29

library(readxl)
library(tidyverse)
library(writexl)

groups <- read_excel("przewalski/data/groups/2011.xlsx", sheet = "09.21")

groups <- groups %>% 
  select(!(contains('...') | contains('bach')))

groups <- subset(groups, select = -16)

colnames(groups) <- as.character(unlist(groups[1,]))

groups_exp <- gather(groups, harem, name, c(1:15) )
groups_exp <- drop_na(groups_exp)
groups_exp$date<-rep((as.Date("2011-09-21")),times=(nrow(groups_exp)))

groups_exp$harem = stri_trans_general(str = groups_exp$harem, id = "Latin-ASCII")
groups_exp$name = stri_trans_general(str = groups_exp$name, id = "Latin-ASCII")

write_xlsx(groups_exp,"przewalski/data/groups/long/2011-09-21.xlsx")

total <- rbind(total,groups_exp)

write_xlsx(total, "przewalski/data/groups/long/total_current")
