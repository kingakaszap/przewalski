# 2023 - last one whoa
library(tidyverse)
library(readxl)
library(stringr)
library(writexl)

# corrections in the total-----
total_current_2022$name <- gsub("UKFEM2022_Nozi1", "Zara", total_current_2022$name)
total_current_2022$name <- gsub("UKFEM2021_Kerecsen_1", "Vanilia", total_current_2022$name)
total_current_2022$name <- gsub("UKFEM2021_Paratlan1", "Zulejka", total_current_2022$name)
total_current_2022$name <- gsub("UKST2022_111", "Sharif", total_current_2022$name)
total_current_2022$harem <- gsub("UKST2022_111", "Sharif", total_current_2022$harem)
total_current_2022$name <- gsub("UKFEM2022_111", "Zeller", total_current_2022$name)
total_current_2022$name <- gsub("UKFEM2022_Nobel1", "Zara", total_current_2022$name)
total_current_2022$name <- gsub("UKST2020815_84", "Rajt", total_current_2022$name)
total_current_2022$harem <- gsub("UKST2020815_84", "Rajt", total_current_2022$harem)
total_current_2022$name <- gsub("UKHST2020_alhatar", "Rajt", total_current_2022$name)
total_current_2022$harem <- gsub("UKHST2020_alhatar", "Rajt", total_current_2022$harem)
total_current_2022$name <- gsub("UKFEM2021_Paratlan2", "Ugyes", total_current_2022$name)
total_current_2022$name <- gsub("UKFEM2021_Paratlan", "Zulejka", total_current_2022$name)
total_current_2022$name <- gsub("UKST2021_89", "Samu", total_current_2022$name)
total_current_2022$harem <- gsub("UKST2021_89", "Samu", total_current_2022$harem)
total_current_2022$name <- gsub("UKFEM2022_Rosalinde1", "Zanot", total_current_2022$name)
total_current_2022$name <- gsub("UKFEM2022_Rosalinde2", "Zselyke", total_current_2022$name)
total_current_2022$harem <- gsub("UKST2022_93", "Orbit", total_current_2022$harem)
total_current_2022$name <- gsub("UKST2022_93", "Orbit", total_current_2022$name)

write_xlsx(total_current_2022, "przewalski/data/groups/long/total_current_2022.xlsx")



# UKFEM2022_Nozi1 -> Zara
# UKFEM2021_Kerecsen_1 -> Vanilia
# UKFEM2021_Paratlan1 -> Zulejka
# UKST2022_111 -> Sharif
# UKFEM2022_111 -> Zeller
# UKFEM2022_Nobel1 -> Zara
# UKST2020_84 -> Rajt !! probably changed this, check - pipa sztm
# UKST2020815_84 -> Rajt 
# UKHST2020_alhatar -> Rajt
# UKFEM2021_Paratlan2 -> Ugyes # Check in 2021 only present from 2022 06/05
# UKFEM2021_Paratlan -> Zulejka # same as above
# both the above are ok i was just stupid
# UKST2021_89 -> Samu - 
# UKFEM2022_Rosalinde1 -> Zanot
# UKFEM2022_Rosalinde2 -> Zselyke
# UKST2022_93 -> Orbit






# Zuljeka_UKFEM2021_Paratlan1 -> Zulejka
# Culla (♀)
# C Zseneroz (♀)


# contains ismeretlenek | Tipp

# everything else----
process_sheet <- function(file_name, sheet_name) {
  groups <- read_excel(file_name, sheet = sheet_name)
  
  groups <- groups %>% slice(1:27)
  
  groups <- groups %>% 
    select(!(contains('...') | (contains('ismeretlenek')|contains('Tipp'))))
  
  colnames(groups) <- as.character(unlist(groups[1,]))
  
  groups<- groups %>% select(!contains('bach'))
  
  groups_exp <- gather(groups, harem, name)
  groups_exp <- drop_na(groups_exp)
  
  
  groups_exp <- groups_exp %>%
    mutate_all(~ str_replace_all(., fixed("Zuljeka_UKFEM2021_Paratlan1"), "Zulejka")) %>% 
    mutate_all(~ str_replace_all(., fixed("Culla (♀)"), "Csoki")) %>%
    mutate_all(~ str_replace_all(., fixed("C Zseneroz (♀)"), "Csoda")) 
  
  
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

total_2023 <- data.frame()  # Initialize an empty data frame to store the total

# Get the Excel file name 
excel_file <- "2023.xlsx"

# Get the sheet names
all_sheets <- excel_sheets(paste0("przewalski/data/groups/", excel_file))

# Process each sheet
for (sheet_name in all_sheets) {
  sheet_data <- process_sheet(paste0("przewalski/data/groups/", excel_file), sheet_name)
  total_2023 <- rbind(total_2023, sheet_data)
}

View(total_2023)
unique(total_2022$date)

total_current_2023<- rbind(total_current_2022, total_2023)

View(total_current_2022)

write_xlsx(total_current_2023, "przewalski/data/groups/long/total_current_2023.xlsx")
# accidentally also overwrote total 2021 whoops