library(readxl)
library(tidyverse)
library(writexl)

group_composition <- read_excel("przewalski/data/groups_example.xlsx")

groups<- group_composition %>% 
  select(!(contains('...') | contains('bach')))
 

groups <- groups [-c(19:196),]
groups[,1:28]<-apply(groups[,1:28],2,function(x){as.character(sub(" *\\(.*","",x))})
colnames(groups) <- as.character(unlist(groups[1,]))

groups$Kerecsen<-gsub("\\?","",(groups$Kerecsen))

groups_exp <- gather(groups, group_name, name, c(1:28) )
groups_exp <- drop_na(groups_exp)

groups_exp$date<-rep((as.Date("2021-01-16")),times=(nrow(groups_exp)))

groups_exp<- groups_exp %>%  
  mutate(year = format(as.Date (date, format="%Y-%m-%d"),"%Y"),
         month = format(as.Date (date, format = "%Y-%m-%d"), "%m"),
         day = format (as.Date (date, format = "%Y-%m-%d"), "%d")) %>% 
  mutate_at(c("year","month","day"), as.numeric)

write_xlsx(groups_exp,"przewalski/data/groups_new.xlsx")

# groups_exp[,1:length(groups_exp)]<-apply(groups_exp[,1:length(groups_exp)],2,function(x){as.character(sub("*\\(.*","",x))})
