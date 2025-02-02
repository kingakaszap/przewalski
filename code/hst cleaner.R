# more data cleaning
# 2024-01-31
library(writexl)

# fixing HSt names
unique(total$harem)
# F soos
total$harem <- gsub("FSoos", "Fsoos", total$harem)
total$harem <- gsub("F.Soos", "Fsoos", total$harem)
total$harem <- gsub("F. Soos", "Fsoos", total$harem)
total$name <- gsub("FSoos", "Fsoos", total$name)
total$name <- gsub("F.Soos", "Fsoos", total$name)
total$name <- gsub("F. Soos", "Fsoos", total$name)
# ukm-100501
total$harem <- gsub("ukm-100501", "UKST2010_Katica", total$harem)
total$name <- gsub("ukm-100501", "UKST2010_Katica", total$name)


# UK20150223_Ashnai
total$harem <- gsub("UK20150223_Ashnai", "UKST2015_Ashnai", total$harem)
total$name <- gsub("UK20150223_Ashnai", "UKST2015_Ashnai", total$name)

# Levente
total$harem <- gsub("Levente?", "Levente", total$harem)
total$name <- gsub("Levente?", "Levente", total$name)

# Kuvik
total$harem <- gsub("Kuvik?", "Kuvik", total$harem)
total$name <- gsub("Kuvik?", "Kuvik", total$name)
total<- total %>% 
  mutate_all(~ str_replace_all(., fixed("Kuvik?"), "Kuba")) 

# UK_bach
total$harem <- gsub("UK_bach", "UKST2015_Omnia", total$harem)
total$name <- gsub("UK_bach", "UKST2015_Omnia", total$name)

# "UK20151229A_Csenge"
# but 3 stallions with Csenge!
# will have to remove those obs-s where HSt is only Hst for 1-2 months
total$harem <- gsub("UK20151229A_Csenge", "UKST2015_Csenge", total$harem)
total$name <- gsub("UK20151229A_Csenge", "UKST2015_Csenge", total$name)

# UK20160130_Honorka
total$harem <- gsub("UK20160130_Honorka", "UKST2016_Honorka", total$harem)
total$name <- gsub("UK20160130_Honorka", "UKST2016_Honorka", total$name)

"10. UNK" 
total$harem <- gsub("10. UNK", "UKST2016_Galagonya", total$harem)
total$name <- gsub("10. UNK", "UKST2016_Galagonya", total$name)

"Lazado  (8)"
total$harem <- gsub("Lazado  (8)", "Lazado", total$harem)
total$name <- gsub("Lazado  (8)", "Lazado", total$name)

# UK20170310_Oazis
total$harem <- gsub("UK20170310_Oazis", "UKST2017_Oazis", total$harem)
total$name <- gsub("UK20170310_Oazis", "UKST2017_Oazis", total$name)

# UK20170813
total$harem <- gsub("UK20170813", "UKST2017_Uszony", total$harem)
total$name <- gsub("UK20170813", "UKST2017_Uszony", total$name)

# "Huba (13"
total<- total %>% 
  mutate_all(~ str_replace_all(., fixed("Huba (13" ), "Huba")) 

total$harem <- gsub("\\?|\\([^)]+\\)", "", total$harem)
unique(total$harem)

# ekes
total<- total %>% 
  mutate_all(~ str_replace_all(., fixed("ekes" ), "Ekes")) 

# UK20150520_Malyva
total<- total %>% 
  mutate_all(~ str_replace_all(., fixed("UK20150520_Malyva" ), "UKST2015_Malyva"))

# "UKHST2018_nefejelcs" 
total<- total %>% 
  mutate_all(~ str_replace_all(., fixed( "UKHST2018_nefejelcs" ), "UKHST2018_nefelejcs"))

# "UK20160331_Ohara"
total<- total %>% 
  mutate_all(~ str_replace_all(., fixed("UK20160331_Ohara" ), "UKST2016_Ohara"))

# "Levente_M,B,,,,"
total<- total %>% 
  mutate_all(~ str_replace_all(., fixed("Levente_M,B,,,,"), "Levente"))

# "Nobel_16.unk.M"
total<- total %>% 
  mutate_all(~ str_replace_all(., fixed("Nobel_16.unk.M"), "Nobel"))

# "Orkan_21.unk.M" 
total<- total %>% 
  mutate_all(~ str_replace_all(., fixed("Orkan_21.unk.M"), "Orkan"))

# "Ozirisz_27.unk.M"
total<- total %>% 
  mutate_all(~ str_replace_all(., fixed("Ozirisz_27.unk.M"), "Ozirisz"))

# "Paratlan_33.unk."
total<- total %>% 
  mutate_all(~ str_replace_all(., fixed("Paratlan_33.unk."), "Paratlan"))

# "Kaloz_34."
total<- total %>% 
  mutate_all(~ str_replace_all(., fixed("Kaloz_34."), "Kaloz"))

# "Piro_36."
total<- total %>% 
  mutate_all(~ str_replace_all(., fixed("Piro_36."), "Piro"))

# "84.unk"
total<- total %>% 
  mutate_all(~ str_replace_all(., fixed("84.unk"), "Rajt"))

# Rajtunk
total<- total %>% 
  mutate_all(~ str_replace_all(., fixed("Rajtunk"), "Rajt"))

# still some to do!

# "Unkmalefelh2020"
total<- total %>% 
  mutate_all(~ str_replace_all(., fixed("Unkmalefelh2020"), "UKHST2020_felho"))

# "Unkmaleszaz2020"
total<- total %>% 
  mutate_all(~ str_replace_all(., fixed("Unkmaleszaz2020"), "UKHST2020_szazszorszep"))

# Marakesh
total<- total %>% 
  mutate_all(~ str_replace_all(., fixed("Marakesh"), "Marrakesh"))

# bach - bachelor groups remained in the dataset by accident
total <- total %>% filter(harem!="bach")

# "UKST202107_88" is the same as "UKST2021_88"
total<- total %>% 
  mutate_all(~ str_replace_all(., fixed("UKST202107_88"), "UKST2021_88"))

# "UKST202109_90" present as "UKST2021_90"
total<- total %>% 
  mutate_all(~ str_replace_all(., fixed("UKST202109_90"), "UKST2021_90"))

# PLOT TWIST this is all the same horse his name is Parizs
total<- total %>% 
  mutate_all(~ str_replace_all(., fixed("UKST2021_90"), "Parizs")) %>% 
  mutate_all(~ str_replace_all(., fixed("Parizs_UKST2021_90"), "Parizs"))

# saving clean dataset

write_xlsx(total, "przewalski/data/groups/long/total_hst_clean.xlsx")

# inasnal: random columns accidentally remained in the dataset
total <- total %>% filter(harem!="inasnal")

unique(total$harem)
# is probably fine

total <- read_xlsx ("przewalski/data/groups/long/total_hst_clean.xlsx")

View(total)
str(total)
total$date<-as.Date(total$date)

# leander to oroszlan
total <- total %>%
  mutate(name = if_else(date > as.Date("2020-03-05") & name == "Leander", "Oroszlan", name),
         harem = if_else(date > as.Date("2020-03-05") & harem == "Leander", "Oroszlan", harem))

# hopefully this worked?

 write_xlsx (total, "przewalski/data/groups/long/total_hst_clean.xlsx")

 
