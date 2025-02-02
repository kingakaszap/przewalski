# group composition - 2009/6/1
# 2023-12-27

library(readxl)
library(tidyverse)
library(writexl)
library(stringi)

groups <- read_excel("przewalski/data/groups/2009.xlsx", sheet = "06.01")

groups <- groups %>% 
  select(!(contains('...') | contains('bach')))

groups <- subset(groups, select = -1)

colnames(groups) <- as.character(unlist(groups[1,]))

groups_exp <- gather(groups, harem, name, c(1:12) )
groups_exp <- drop_na(groups_exp)
groups_exp$date<-rep((as.Date("2009-06-01")),times=(nrow(groups_exp)))

write_xlsx(groups_exp,"przewalski/data/groups/long/2009-06-01.xlsx")

total <- read_excel("przewalski/data/groups/long/total_current.xlsx")
april <-read_excel("przewalski/data/groups/long/2008-04.xlsx")
jan <- read_excel("przewalski/data/groups/long/2008-01.xlsx")
may <- read_excel("przewalski/data/groups/long/2008-05.xlsx")
june_1 <-read_excel("przewalski/data/groups/long/2008-06-01.xlsx")
june_2 <-read_excel("przewalski/data/groups/long/2008-06-10.xlsx")
june_3 <-read_excel("przewalski/data/groups/long/2008-06-30.xlsx")


total <- rbind(jan,april,may,june_1,june_2,june_3,groups_exp)



total$harem = stri_trans_general(str = total$harem, id = "Latin-ASCII")
total$name = stri_trans_general(str = total$name, id = "Latin-ASCII")

write_xlsx(total, "przewalski/data/groups/long/total_current.xlsx")
