library(readxl)
library(tidyverse)
groups <- read_excel("C:/Users/Kinga/Downloads/Group composition 2021_jav2_Viki (1).xlsx")

# selecting the columns which are not empty - is there an automatic way to do this?
groups <- groups %>% 
  select(c("1.", "2.", "3.", "4.", "5.", "6.", "7.", "8.",
           "9.", "10.", "11.", "12.", "13.", "14.", "15.", "16.", "17.")) 


# removing rows which only have notes

groups <- groups [-c(19:195),]
# removing everything after the bracket

groups[,1:17]<-apply(groups[,1:17],2,function(x){as.character(sub(" *\\(.*","",x))})

# making the first row (name of hst) the column name
colnames(groups) <- as.character(unlist(groups[1,]))
groups = groups[-1 ]

# saving file and manually removing some things - could be done before
# or if there is a way to remove everything that comes after an NA in R

library("writexl")
write_xlsx(groups,"C:\\Users\\Kinga\\Documents\\R_ideiglenes\\groups_1_temporary.xlsx")
groups <- read_excel("C:\\Users\\Kinga\\Documents\\R_ideiglenes\\groups_1_temporary.xlsx")


names(groups)

# long format

groups_exp <- gather(groups, group_name, name, c(1:16) )
groups_exp <- drop_na(groups_exp)


groups_exp %>% 
  filter(group_name == "Hollo") %>% 
  nrow()
