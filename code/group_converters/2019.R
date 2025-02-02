# 2019
# 2024-01-20

library(readxl)
library(tidyverse)
library(writexl)

process_sheet <- function(file_name, sheet_name) {
  groups <- read_excel(file_name, sheet = sheet_name)
  
  groups <- groups %>% 
    select(!(contains('...') | contains('bach')))
  
  colnames(groups) <- as.character(unlist(groups[1,]))
  
  groups_exp <- gather(groups, harem, name)
  groups_exp <- drop_na(groups_exp)
  
  date_column <- as.Date(sheet_name, format = "%Y-%m-%d")
  
  groups_exp <- groups_exp %>% 
    mutate(date = rep(date_column, times = nrow(groups_exp)),
           harem = stri_trans_general(str = groups_exp$harem, id = "Latin-ASCII"),
           name = stri_trans_general(str = groups_exp$name, id = "Latin-ASCII")) %>% 
    mutate(name = trimws(str_replace(name, "\\(.*?\\)", "")), 
           harem = trimws(str_replace(harem, "\\(.*?\\)", "")))
  
  groups_exp$harem <- gsub("\\?|\\([^)]+\\)", "", groups_exp$harem)
  groups_exp$name <- gsub("\\?|\\([^)]+\\)", "", groups_exp$name)
  
  groups_exp$name <- gsub("Truffelvagykanca", "Truffel", groups_exp$name)
  
  
  write_xlsx(groups_exp, paste0("przewalski/data/groups/long/", sheet_name, ".xlsx"))
  
  return(groups_exp)
}

total_2019 <- data.frame()  # Initialize an empty data frame to store the total

# get name
excel_file <- "2019.xlsx"

# get sheet names
all_sheets <- excel_sheets(paste0("przewalski/data/groups/", excel_file))

# Process each sheet
for (sheet_name in all_sheets) {
  sheet_data <- process_sheet(paste0("przewalski/data/groups/", excel_file), sheet_name)
  total_2019 <- rbind(total_2019, sheet_data)
}

View(total_2019)


total_current_2019<- rbind(total_current_2018, total_2019)

View(total_current_2019)

write_xlsx(total_current_2019, "przewalski/data/groups/long/total_current_2019.xlsx")
