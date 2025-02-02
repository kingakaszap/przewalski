# 2022 
library(tidyverse)
library(readxl)
library(writexl)
library(stringr)

# corrections for the total:
# in total : UKST2021_90 -> Parizs
# Pöszméte_UKST2022_91  -> Poszmete - dont know why i thought i need this, its a 2022 stallion.
total_current_2021$harem <- gsub("UKST2021_90", "Parizs", total_current_2021$harem)
total_current_2021$name <- gsub("UKST2021_90", "Parizs", total_current_2021$name)



process_sheet <- function(file_name, sheet_name) {
  groups <- read_excel(file_name, sheet = sheet_name)
  
  groups <- groups %>% slice(1:27)
  
  groups <- groups %>% 
    select(!(contains('...') ))
  
  colnames(groups) <- as.character(unlist(groups[1,]))
  
  groups<- groups %>% select(!contains('bach'))
  
  groups_exp <- gather(groups, harem, name)
  groups_exp <- drop_na(groups_exp)
  
  
  groups_exp <- groups_exp %>%
    mutate_all(~ str_replace_all(., fixed("K,V,K,?,?Fi"), "UKFEM2022_Roham")) %>% 
  mutate_all(~ str_replace_all(., fixed("NemSelymes"), "UKFEM2021_Kope")) %>%
    mutate_all(~ str_replace_all(., fixed("Oroszlán_hiányos bal fülhegy"), "Oroszlan")) %>% 
    mutate_all(~ str_replace_all(., fixed("Paprika_UKST202107_88 (3)"), "Paprika")) %>%  
    mutate_all(~ str_replace_all(., fixed("Clelle"), "Csello")) %>% 
    mutate_all(~ str_replace_all(., fixed(" Párizs_UKST2021_90 (3)"), "Parizs")) %>%
    mutate_all(~ str_replace_all(., fixed("Pöszméte_UKST2022_91 (3)"), "Poszmete")) %>%
    mutate_all(~ str_replace_all(., fixed("Cremek"), "Celeb")) %>%
    mutate_all(~ str_replace_all(., fixed("Cvojtina"), "Cuki")) %>% 
    mutate_all(~ str_replace_all(., fixed("Cprimula"), "Csanad")) %>%
    mutate_all(~ str_replace_all(., fixed("Curom"), "Cickafark")) %>%
    mutate_all(~ str_replace_all(., fixed("C zanza"), "Cezar")) %>%
    mutate_all(~ str_replace_all(., fixed("C Zanza (♂)"), "Cezar")) %>%
    mutate_all(~ str_replace_all(., fixed("C Zserbó"), "UKFOAL2022_P3")) %>%
    mutate_all(~ str_replace_all(., fixed("C Zseneroz"), "Csoda")) %>%
    mutate_all(~ str_replace_all(., fixed("Cniké"), "Chaos")) %>%
    mutate_all(~ str_replace_all(., fixed("Culla"), "Csoki")) %>%
    mutate_all(~ str_replace_all(., fixed("Cuadzset"), "Colt")) %>%
    mutate_all(~ str_replace_all(., fixed("C Kála"), "Csarab")) %>%
    mutate_all(~ str_replace_all(., fixed("UKFEM2022_119"), "Telma")) %>%
    mutate_all(~ str_replace_all(., fixed("Cukfem2022_119"), "Czinege")) %>% 
  mutate_all(~ str_replace_all(., fixed("Ctelma"), "Telma")) %>%
    mutate_all(~ str_replace_all(., fixed("C Mazsola"), "Csubi"))
    
  
  date_column <- as.Date(sheet_name, format = "%Y-%m-%d")
  
  
  
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

total_2022 <- data.frame()  # Initialize an empty data frame to store the total

# Get the Excel file name (assumed to be a year)
excel_file <- "2022.xlsx"

# Get the sheet names
all_sheets <- excel_sheets(paste0("przewalski/data/groups/", excel_file))

# Process each sheet
for (sheet_name in all_sheets) {
  sheet_data <- process_sheet(paste0("przewalski/data/groups/", excel_file), sheet_name)
  total_2022 <- rbind(total_2022, sheet_data)
}

View(total_2022)
unique(total_2022$date)

total_current_2022<- rbind(total_current_2021, total_2022)

View(total_current_2022)

write_xlsx(total_current_2022, "przewalski/data/groups/long/total_current_2022.xlsx")
# accidentally also overwrote total 2021 whoops

# K,V,K,?,?Fi -> UKFEM2022_Roham
# Oroszlán_hiányos bal fülhegy -> Oroszlan
# Paprika_UKST202107_88 (3) -> Paprika
# Clelle -> Csello
# Párizs_UKST2021_90 (3) -> Parizs
# Pöszméte_UKST2022_91 (3) -> Poszmete
# UKST2022_91 (3) -> Poszmete
# UKST2021_90 (3) -> Parizs
# Cremek -> Celeb
# Cvojtina -> Cuki
# Cprimula -> Csanad
# Curom -> Cickafark
#C zanza -> Cezar
# C Zserbó -> UKFOAL2022_P3
# C Zseneroz -> Csoda
#Cniké -> Chaos
#Culla -> Csoki
# Cuadzset -> Colt
# C Kála -> Csarab
# UKFEM2022_119 -> Telma
# Cukfem2022_119 -> Czinege
# Ctelma -> Czinege
# C Mazsola -> Csubi









