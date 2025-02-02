# group composition - 2008/4
# 2023-11-10

library(readxl)
library(tidyverse)
library(writexl)

groups <- read_excel("przewalski/data/groups/2008.xlsx", sheet = "04.01")

groups <- groups %>% 
  select(!(contains('...') | contains('bach')))

groups <- subset(groups, select = -1)

colnames(groups) <- as.character(unlist(groups[1,]))

groups_exp <- gather(groups, harem, name, c(1:10) )
groups_exp <- drop_na(groups_exp)
groups_exp$date<-rep((as.Date("2008-04-01")),times=(nrow(groups_exp)))

write_xlsx(groups_exp,"przewalski/data/groups/long/2008-04.xlsx")

april <- read_excel("przewalski/data/groups/long/2008-04.xlsx")
jan <-read_excel("przewalski/data/groups/long/2008-01.xlsx")

total <- rbind(april, jan)
