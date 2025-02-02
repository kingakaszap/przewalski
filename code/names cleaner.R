# more cleaning - mares
library(readxl)
library(writexl)
library(tidyverse)

total <- read_xlsx ("przewalski/data/groups/long/total_hst_clean.xlsx")
unique(total$name)

# GOSH so many issues

# kokeny(csilla)
total <- total %>%
  mutate_all(~ str_replace_all(., fixed("Kokeny (Csilla)"), "Kokeny"))
# ozike, oszton, ocean, oda, ohaj
total$name <- gsub("oszton", "Oszton", total$name)
total$name <- gsub("ozike", "Ozike", total$name)
total$name <- gsub("ocean", "Ocean", total$name)
total$name <- gsub("oda", "Oda", total$name)
total$name <- gsub("ozon", "Ozon", total$name)
total$name <- gsub("odon", "Odon", total$name)
total$name <- gsub("ohaj", "Ohaj", total$name)


# unknown - hetenynel 2012
total$name <- gsub("unknown", "UKFEM2012_Heteny", total$name)

# jogurt
total$name <- gsub("Jogurt", "Joghurt", total$name)


# foals in 2008
total$name <- gsub("Flora's foal", "Kristaly", total$name)

total$name <- gsub("Himes' colt", "Kalandor", total$name)
total$name <- gsub("Gal's filly", "Katica", total$name)
total$name <- gsub("Fanny's colt", "Kende", total$name)
total$name <- gsub("Hovirag's filly", "Kincso", total$name)
total$name <- gsub("Greta's colt", "Keszi", total$name)
total$name <- gsub("Eper's colt", "Kuvik", total$name)
total$name <- gsub("Dorka's filly", "Kisasszony", total$name)
total$name <- gsub("Emo's colt", "Kond", total$name)
total$name <- gsub("Emo's  colt", "Kond", total$name)
total$name <- gsub("Gerle's colt", "Koppany", total$name)
total$name <- gsub("Flora's filly" , "Kristaly", total$name)
total$name <- gsub("Gizella's colt" , "Kerecsen", total$name)
total$name <- gsub("Dicso's colt" , "Kankalin", total$name)
total$name <- gsub("Dicso's filly" , "Kankalin", total$name)
total$name <- gsub("Epona's colt" , "Kitan", total$name)
total$name <- gsub("Sjilka's filly",  "Kala", total$name)
total$name <- gsub("Felho's filly",  "Kecses", total$name)
total$name <- gsub("Dalma's colt",  "Kanya", total$name)
total$name <- gsub("Hargita's filly",  "Kikerics", total$name)

total$name <- gsub("foal", "Kankalin", total$name)

# 38
total<- total %>% filter(name!="38")

# marrakesh
total$name <- gsub("Marakes",  "Marrakesh", total$name)

# pzp - remove from everywhere
total$name <- gsub("\\(pzp\\)", "", total$name)

# Kisasszony
total$name <- gsub("Kissasszony" ,  "Kisasszony", total$name)

# bachelors (1-3) in Geza group in 2010 - removed, no further/previous records of them
total<- total %>% filter(name!="bachelors(1-3)")

# missing: harmat, mormota in 2013 - removed
total<- total %>% filter(name!="Missing:Harmat, Mormota")

# ? remove from everywhere
total$name <- gsub("\\?", "", total$name)


# rosa missing - remove (irha 2015)
total<- total %>% filter(name!="Rosamissing")

# rege died - remove (present on dates after alleged death acc to nagylista)
total<- total %>% filter(name!="Rege Died")

# remove dead foals lol
total<- total %>% filter(name!="R...Died")
total<- total %>% filter(name!="ReggelDied")

# rejtek missing - remove
total<- total %>% filter(name!="Rejtekmissing")

# "UK20150420_JakabO" 
total$name <- gsub("UK20150420_JakabO" ,  "UK20150420_Jakab", total$name)

# "injury on the head" - ??
total<- total %>% filter(name!="injury on the head")

#  "S_noemi_died♀)" 
total$name <- gsub("S_noemi_died♀)" ,  "S_noemi_died", total$name)

# lazado
total <- total %>%
  mutate_all(~ str_replace_all(., fixed( "Lazado  (8)"), "Lazado"))

# R..
# ended up adding the two R foals by hand: 2014-06-06, to Hajmas group.

# Ribanna died
total$name <- gsub("Ribannadied" ,  "Ribanna", total$name)

# Unk.T..♂dark
total <- total %>%
  mutate_all(~ str_replace_all(., fixed( "Unk.T..♂dark"), "UK20170812_GezaT"))
total <- total %>%
  mutate_all(~ str_replace_all(., fixed( "UK20170812_GezaT"), "UKFEM20170812_GezaT"))

total <- total %>%
  mutate_all(~ str_replace_all(., fixed( "Wolf♂)"), "Wolf"))

# truffel vagy kanca - thought i cleaned this up in 2019
total$name <- gsub("Truffelvagykanca" ,  "Truffel", total$name)

# verepes
total$name <- gsub("Verepess" ,  "Verepes", total$name)

# Rahel,K,B,K,,,
total <- total %>%
  mutate_all(~ str_replace_all(., fixed( "Rahel,K,B,K,,,"), "Rahel"))

# "K,V,K,," in jellem 2019
# changed to ragyog bc of previous observation, but this is just a best guess
total <- total %>%
  mutate_all(~ str_replace_all(., fixed( "K,V,K,,"), "Ragyog"))

# angyal
total$name <- gsub("Anagyal" ,  "Angyal", total$name)

# vadoc
total <- total %>%
  mutate_all(~ str_replace_all(., fixed( "Vadoc_83/1_K"), "Vadoc"))

# pemzli
total$name <- gsub("Pemzli8" ,  "Pemzli", total$name)

# random name

total<- total %>% filter(name!="05.26") %>% 
  filter(name!="nv") %>% 
  filter(name!= "Szerintem")

# zulejka
total$name <- gsub("Zulejka3" ,  "Zulejka", total$name)

# ukfem kaloz 2022
total$name <- gsub("ukfem2022_Kaloz" ,  "UKFEM2022_Kaloz", total$name)

# brownie
total$name <- gsub("Browny" ,  "Brownie", total$name)



# remove spaces
total$name <- gsub(" ", "", total$name)


 which(total == "D", arr.ind=TRUE)


write_xlsx(total, "przewalski/data/groups/long/full_clean.xlsx")
