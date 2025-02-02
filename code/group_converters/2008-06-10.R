# group composition - 2008/6/10
# 2023-12-27

library(readxl)
library(tidyverse)
library(writexl)

groups <- read_excel("przewalski/data/groups/2008.xlsx", sheet = "06.10")

groups <- groups %>% 
  select(!(contains('...') | contains('bach')))

groups <- subset(groups, select = -1)

colnames(groups) <- as.character(unlist(groups[1,]))

groups_exp <- gather(groups, harem, name, c(1:9) )
groups_exp <- drop_na(groups_exp)
groups_exp$date<-rep((as.Date("2008-06-10")),times=(nrow(groups_exp)))

write_xlsx(groups_exp,"przewalski/data/groups/long/2008-06-10.xlsx")

may <- read_excel("przewalski/data/groups/long/2008-05.xlsx")
april <-read_excel("przewalski/data/groups/long/2008-04.xlsx")
jan <- read_excel("przewalski/data/groups/long/2008-01.xlsx")
june_1 <- read_excel("przewalski/data/groups/long/2008-06.xlsx")

total <- rbind(jan,april,may,june_1, groups_exp)

write_xlsx(total, "przewalski/data/groups/long/total_current")


