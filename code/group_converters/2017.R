# 2017
# hopefully this works
# 2024-01-15

library(readxl)
library(tidyverse)
library(writexl)

process_sheet <- function(file_name, sheet_name) {
  groups <- read_excel(file_name, sheet = sheet_name)
  
  
  groups <- groups %>% 
    select(!(contains('...') | contains('bach')))
  
  colnames(groups) <- as.character(unlist(groups[1,]))
  
 # groups <- subset(groups, select = -Bachelors)
  
  groups_exp <- gather(groups, harem, name)
  groups_exp <- drop_na(groups_exp)
  
  date_column <- as.Date(sheet_name, format = "%Y-%m-%d")
  
  groups_exp <- groups_exp %>% 
    mutate(date = rep(date_column, times = nrow(groups_exp)),
           harem = stri_trans_general(str = groups_exp$harem, id = "Latin-ASCII"),
           name = stri_trans_general(str = groups_exp$name, id = "Latin-ASCII")) %>% 
    mutate(name = trimws(str_replace(name, "\\(.*?\\)", "")), 
           harem = trimws(str_replace(harem, "\\(.*?\\)", "")))
  
  write_xlsx(groups_exp, paste0("przewalski/data/groups/long/", sheet_name, ".xlsx"))
  
  return(groups_exp)
}

total_2017 <- data.frame()  # Initialize an empty data frame to store the total

# Get the Excel file name (assumed to be a year)
excel_file <- "2017.xlsx"

# Get the sheet names
all_sheets <- excel_sheets(paste0("przewalski/data/groups/", excel_file))

# Process each sheet
for (sheet_name in all_sheets) {
  sheet_data <- process_sheet(paste0("przewalski/data/groups/", excel_file), sheet_name)
  total_2017 <- rbind(total_2017, sheet_data)
}

View(total_2017)

unique(total_2017$date)

total_current_2017<- rbind(total_2016, total_2017)

View(total_current_2017)

write_xlsx(total_current_2017, "przewalski/data/groups/long/total_current_2017.xlsx")
