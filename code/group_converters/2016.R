library(readxl)
library(tidyverse)
library(writexl)


total_current<- total_current %>% 
  filter(!grepl('2016', date))
View(total_current)

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

total <- data.frame()  # Initialize an empty data frame to store the total
date_values <- c()  # Initialize a vector to store date values

# Get the Excel file name (assumed to be a year)
excel_file <- "2016.xlsx"

# Get the sheet names
all_sheets <- excel_sheets(paste0("przewalski/data/groups/", excel_file))

# Process each sheet
for (sheet_name in all_sheets) {
  sheet_data <- process_sheet(paste0("przewalski/data/groups/", excel_file), sheet_name)
  total_2016 <- rbind(total_current, sheet_data)
  date_values <- c(date_values, as.Date(sheet_name, format = "%Y-%m-%d"))
}

# Add the date values to the total data frame
total_2016$date <- date_values

# Save the combined total to a new Excel file with one sheet named "total"
write_xlsx(list(total = total), paste0("przewalski/data/groups/long/total_", tools::file_path_sans_ext(basename(excel_file)), ".xlsx"))





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

total_2016 <- data.frame()  # Initialize an empty data frame to store the total

# Get the Excel file name (assumed to be a year)
excel_file <- "2016.xlsx"

# Get the sheet names
all_sheets <- excel_sheets(paste0("przewalski/data/groups/", excel_file))

# Process each sheet
for (sheet_name in all_sheets) {
  sheet_data <- process_sheet(paste0("przewalski/data/groups/", excel_file), sheet_name)
  total_2016 <- rbind(total_current, sheet_data)
}

View(total_2016)

write_xlsx(total_2016, "przewalski/data/groups/long/total_2016.xlsx")


# Save the combined total to a new Excel file with one sheet named "total"



write_xlsx(list(total = total), paste0("przewalski/data/groups/long/total_", tools::file_path_sans_ext(basename(excel_file)), ".xlsx"))


process_sheet <- function(sheet_name) {
  groups <- read_excel("przewalski/data/groups/2016.xlsx", sheet = sheet_name)
  
  groups <- groups %>% 
    select(!(contains('...') | contains('bach')))
  
  colnames(groups) <- as.character(unlist(groups[1,]))
  
  groups <- subset(groups, select = -Bachelors)
  
  groups_exp <- gather(groups, harem, name)
  groups_exp <- drop_na(groups_exp)
  
  groups_exp <- groups_exp %>% 
    mutate(date = rep(as.Date(sheet_name, format = "%Y-%m-%d"), times = nrow(groups_exp)),
           harem = stri_trans_general(str = groups_exp$harem, id = "Latin-ASCII"),
           name = stri_trans_general(str = groups_exp$name, id = "Latin-ASCII")) %>% 
    mutate(name = trimws(str_replace(name, "\\(.*?\\)", "")), 
           harem = trimws(str_replace(harem, "\\(.*?\\)", "")))
  
  write_xlsx(groups_exp, paste0("przewalski/data/groups/long/", sheet_name, ".xlsx"))
  
  return(groups_exp)
}

total_current <- data.frame()  # Initialize an empty data frame to store the total

# Get the sheet names
all_sheets <- excel_sheets("przewalski/data/groups/2016.xlsx")

# Process each sheet
for (sheet_name in all_sheets) {
  sheet_data <- process_sheet(sheet_name)
  total_current <- rbind(total, sheet_data)
}

# Save the combined total to a new Excel file with one sheet named "total"
write_xlsx(list(total_current = total_current), "przewalski/data/groups/long/total_current.xlsx")
