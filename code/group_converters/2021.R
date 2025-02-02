# 2021

# corrections
total_current_2020$harem <- gsub("UKHST2020_szazszorszep", "Pegazus", total_current_2020$harem)
total_current_2020$name <- gsub("UKHST2020_szazszorszep", "Pegazus", total_current_2020$name)
total_current_2020$name <- gsub("UKFEM2020_UKHST2020_alhatar", "Ulla", total_current_2020$name)
write_xlsx(total_current_2020, "przewalski/data/groups/long/total_current_2020.xlsx")

# Correct Ulla before!!
# es pegazust is
# NemSelymes - UKFEM2021_Kope

# UKMALE2021_97 - UKST2021_Pumukli
#UKST2021_88(3) - UKHST2021_Unikornis
#UKST202109_90 (3) - UKHST2021_Telma
# UKST2021_89 - UKHST2021_Rendetlen

process_sheet <- function(file_name, sheet_name) {
  groups <- read_excel(file_name, sheet = sheet_name)
  
  groups <- groups %>% slice(1:27)
  
  
  groups <- groups %>% 
    select(!(contains('...') | contains('bach')))
  
  colnames(groups) <- as.character(unlist(groups[1,]))
  
  groups_exp <- gather(groups, harem, name)
  groups_exp <- drop_na(groups_exp)
  
  # Assuming your dataframe is named 'groups_exp'
  # and 'dplyr' and 'stringr' packages are installed
  library(dplyr)
  library(stringr)
  
  groups_exp <- groups_exp %>%
    mutate_all(~ str_replace_all(., fixed("NemSelymes"), "UKFEM2021_Kope")) %>%
    mutate_all(~ str_replace_all(., fixed("UKMALE2021_97"), "UKST2021_Pumukli")) 
  
  date_column <- as.Date(sheet_name, format = "%Y-%m-%d")
  
  groups_exp$harem <- gsub("\\s.*|\\(.*|,.*|!.*", "", groups_exp$harem)
  groups_exp$name <- gsub("\\s.*|\\(.*|,.*|!.*", "", groups_exp$name)
  
  
  groups_exp <- groups_exp %>% 
    mutate(date = rep(date_column, times = nrow(groups_exp)),
           harem = stri_trans_general(str = groups_exp$harem, id = "Latin-ASCII"),
           name = stri_trans_general(str = groups_exp$name, id = "Latin-ASCII")) %>% 
    mutate(name = trimws(str_replace(name, "\\(.*?\\)", "")), 
           harem = trimws(str_replace(harem, "\\(.*?\\)", "")))
  
  groups_exp$harem <- gsub("\\?|\\([^)]+\\)", "", groups_exp$harem)
  groups_exp$name <- gsub("\\?|\\([^)]+\\)", "", groups_exp$name)
  
  
  write_xlsx(groups_exp, paste0("przewalski/data/groups/long/", sheet_name, ".xlsx"))
  
  return(groups_exp)
}

total_2021 <- data.frame()  # Initialize an empty data frame to store the total

# Get the Excel file name (assumed to be a year)
excel_file <- "2021.xlsx"

# Get the sheet names
all_sheets <- excel_sheets(paste0("przewalski/data/groups/", excel_file))

# Process each sheet
for (sheet_name in all_sheets) {
  sheet_data <- process_sheet(paste0("przewalski/data/groups/", excel_file), sheet_name)
  total_2021 <- rbind(total_2021, sheet_data)
}

View(total_2021)
View(total_2019)
unique(total_2021$date)

total_current_2021<- rbind(total_current_2020, total_2021)

View(total_current_2021)

write_xlsx(total_current_2021, "przewalski/data/groups/long/total_current_2021.xlsx")




