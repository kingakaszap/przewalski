library(readxl)
library(tidyverse)
library(writexl)

process_sheet <- function(file_name, sheet_name) {
  groups <- read_excel(file_name, sheet = sheet_name)
  
  
  groups <- groups %>% 
    select(!(contains('...') | contains('bach')))
  
  colnames(groups) <- as.character(unlist(groups[1,]))
  
  groups <- subset(groups, select = -Bachelors)
  
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

total_current <- data.frame()  # Initialize an empty data frame to store the total
date_values <- data.frame()  # Initialize a data frame to store date values

# Get the Excel file name (assumed to be a year)
excel_file <- "2016.xlsx"

# Get the sheet names
all_sheets <- excel_sheets(paste0("przewalski/data/groups/", excel_file))

# Process each sheet
for (sheet_name in all_sheets) {
  sheet_data <- process_sheet(paste0("przewalski/data/groups/", excel_file), sheet_name)
  total_current <- rbind(total_current, sheet_data)
  date_values <- rbind(date_values, data.frame(date = as.Date(sheet_name, format = "%Y-%m-%d")))
}

View(total_current)

total_2016<- rbind(total_current_2015, total_current)
View(total_2016)

# Add the date values to the total data frame
total <- cbind(total, date_values)

# Save the combined total to a new Excel file with one sheet named "total"
write_xlsx(list(total = total), paste0("przewalski/data/groups/long/total_", tools::file_path_sans_ext(basename(excel_file)), ".xlsx"))
