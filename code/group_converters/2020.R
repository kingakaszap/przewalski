# 2020
# goddamn is this long

# unkfempar2020 (K,B???,FI -> UKFEM2020_paratlan
# unkmaleozi2020(M,V,?,?,(5) -> UKHST2020_ozike
# 1 barna kanca, de nem tudom melyik -> UKFEM2020_kitan
# unkfemlean2020 -> UKFEM2020_leander
# Unkfemhub2020 (K,V,K,?,?,? -> UKFEM2020_huba
# Apiroska -> Arcana
# Unkfemmar12020(_K,B,?,? -> UKFEM2020_marvany
# unkfempir2020 -> UKFEM2020_piro
# Unkfemlan12020(_K,B,F,?,?, Fi -> UKFEM2020_lantos
# Unkmalefelh2020( Nem jellem, erős vállkereszt -> UKHST2020_felho
# 84.unk( M,B,?,?,? ->UKHST2020_alhatar
# Unkfem842020 -> UKFEM2020_UKHST2020_alhatar
# Unkfemlan22020Lan2(_K, B,szürkés, V -> UKFEM2020_lantos2
# Unkmaleszaz2020(. (6) M,V,?,?,? -> UKHST2020_szazszorszep
# Unkmaleszaz2020 (6) 85.unk, M,V,?,?,? -y UKHST2020_szazszorszep


library(readxl)
library(tidyverse)
library(writexl)
library(stringr)

process_sheet <- function(file_name, sheet_name) {
  groups <- read_excel(file_name, sheet = sheet_name)
  
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
    mutate_all(~ str_replace_all(., fixed("unkfempar2020 (K,B???,FI"), "UKFEM2020_paratlan")) %>%
    mutate_all(~ str_replace_all(., fixed("unkmaleozi2020(M,V,?,?,(5)"), "UKHST2020_ozike")) %>%
    mutate_all(~ str_replace_all(., fixed("1 barna kanca, de nem tudom melyik"), "UKFEM2020_kitan")) %>%
    mutate_all(~ str_replace_all(., fixed("unkfemlean2020"), "UKFEM2020_leander")) %>% 
    mutate_all(~ str_replace_all(., fixed("Unkfemhub2020 (K,V,K,?,?,?"), "UKFEM2020_huba")) %>%
    mutate_all(~ str_replace_all(., fixed("Apiroska"), "Arcana")) %>%
    mutate_all(~ str_replace_all(., fixed("Unkfemmar12020(_K,B,?,?"), "UKFEM2020_marvany")) %>%
    mutate_all(~ str_replace_all(., fixed("unkfempir2020"), "UKFEM2020_piro")) %>%
    mutate_all(~ str_replace_all(., fixed("Unkfemlan12020(_K,B,F,?,?, Fi)"), "UKFEM2020_lantos")) %>%
    mutate_all(~ str_replace_all(., fixed("Unkmalefelh2020( Nem jellem, erős vállkereszt"), "UKHST2020_felho")) %>%
    mutate_all(~ str_replace_all(., fixed("84.unk( M,B,?,?,?"), "UKHST2020_alhatar")) %>%
    mutate_all(~ str_replace_all(., fixed("Unkfem842020"), "UKFEM2020_UKHST2020_alhatar")) %>%
    mutate_all(~ str_replace_all(., fixed("Unkfemlan22020Lan2(_K, B,szürkés), V"), "UKFEM2020_lantos2")) %>%
    mutate_all(~ str_replace_all(., fixed("Unkmaleszaz2020(. (6) M,V,?,?,?)"), "UKHST2020_szazszorszep")) %>%
    mutate_all(~ str_replace_all(., fixed("Unkmaleszaz2020 (6) 85.unk, M,V,?,?,?)"), "UKHST2020_szazszorszep"))
  
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

  total_2020 <- data.frame()  # Initialize an empty data frame to store the total
  
  # Get the Excel file name 
  excel_file <- "2020.xlsx"
  
  # Get the sheet names
  all_sheets <- excel_sheets(paste0("przewalski/data/groups/", excel_file))
  
  # Process each sheet
  for (sheet_name in all_sheets) {
    sheet_data <- process_sheet(paste0("przewalski/data/groups/", excel_file), sheet_name)
    total_2020 <- rbind(total_2020, sheet_data)
  }

View(total_2020)
unique(total_2020$date)

total_current_2020<- rbind(total_current_2019, total_2020)

View(total_current_2020)

write_xlsx(total_current_2020, "przewalski/data/groups/long/total_current_2020.xlsx")



