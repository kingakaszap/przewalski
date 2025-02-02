library(readxl)
library(tidyverse)
library(skimr)
library(stringr)

# try importing without headers?


groups <- read_excel("przewalski/data/groups_example.xlsx", 
                             col_names = FALSE)
library(dplyr)
groups_new <- groups %>% select_if(~!is.na(.[1L]))

View(groups_example)

groups <- read_excel("przewalski/data/groups_example.xlsx")
View(groups)
skim(groups)

groups[,1:88]<-apply(groups[,1:88],2,function(x){as.character(sub(" *\\(.*","",x))})

groups <- sapply( function(x) {as.character(sub(" *\\(.*","",x))} )  
 
groups <- groups %>%  sapply( function(x) {as.character(sub(" *\\(.*","",x))} ) 
# After making first rows non-headers
 
# noname <- names(groups) %in% c("")
# groups_short <- groups [! noname]

new.groups <- groups[!grepl(".", colnames(groups)), ]

new.groups <- groups %>% 
  select(-(grepl(colnames, ".")))

new.groups <- groups[!grepl(".",colnames(groups))
                       ]

groups_new <- groups %>% select_if(!grepl( ".", colnames))

groups <- groups %>% 
  select(c("1.", "2.", "3.", "4.", "5.", "6.", "7.", "8.",
           "9.", "10.", "11.", "12.", "13.", "14.", "15.", "16.", "17.")) 


groups <- groups [-c(17:195),]
groups <- groups [-17,]
groups[,1:17]<-apply(groups[,1:17],2,function(x){as.character(sub(" *\\(.*","",x))})
colnames(groups) <- as.character(unlist(groups[1,]))
groups = groups[-1 ]



library("writexl")
write_xlsx(groups,"C:\\Users\\Kinga\\Documents\\R_ideiglenes\\groups_1_temporary.xlsx")
groups <- read_excel("C:\\Users\\Kinga\\Documents\\R_ideiglenes\\groups_1_temporary.xlsx")


groups$Kerecsen

names(groups)

groups_exp <- gather(groups, group_name, name, c(1:16) )
groups_exp <- drop_na(groups_exp)
  