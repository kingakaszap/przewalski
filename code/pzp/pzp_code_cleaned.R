# august 2023
# pzp

# libraries and database ----
library(tidyverse)
library(readxl)
library(janitor)
library(writexl)

pzp_original <- read_excel("przewalski/data/pzp/PZP_original.xlsx")

#----
# sorting out booster 1 and booster 2 columns ----

# booster 1
pzp <- pzp_original %>% 
  mutate( boost_temp_1 = janitor::convert_to_date((as.numeric(Booster1)),
                        character_fun = lubridate::dmy),
          boost_temp_2 = case_when(
            grepl("-", Booster1) ~ Booster1)) %>% 
  mutate(boost_temp_2 = as.Date(boost_temp_2)) %>% 
  mutate(Booster1_updated = case_when (
    !is.na(boost_temp_1) ~ boost_temp_1,
    !is.na(boost_temp_2) ~ boost_temp_2),
    Booster1 = Booster1_updated) %>% 
  dplyr::select (-Booster1_updated)

# booster 2
# i am not sure if we will actually need the status column here.
pzp <- pzp %>% 
  mutate(Booster2_date_temp = janitor::convert_to_date(as.numeric(Booster2),
                                                       character_fun = lubridate::dmy),
         Booster2_status = case_when
         (Booster2 == "OUT" ~ "OUT",
           !is.na(Booster2_date_temp) ~ "YES",
         ),
         Booster2 = Booster2_date_temp)


 
#-----
# sorting out other dates ----
# removes character data from these columns,  i think we dont need it
pzp <- pzp %>% 
  mutate (Booster3 = janitor::convert_to_date(as.numeric(Booster3), character_fun = lubridate::dmy),
         Booster2_again = janitor::convert_to_date(as.numeric(Booster2_again), character_fun = lubridate::dmy),
         DOB_foal_year_before = janitor::convert_to_date(as.numeric(DOB_foal_year_before), character_fun = lubridate::dmy),
         DOB_foal_treatment = janitor::convert_to_date(as.numeric(DOB_foal_treatment), character_fun = lubridate::dmy),
         Booster1_again3 = janitor::convert_to_date(as.numeric(Booster1_again3), character_fun = lubridate::dmy)
         )
pzp <- pzp %>% 
  mutate_if(is.POSIXct, as.Date)

#-----
# births, adding status column (v long and not pretty) ----
pzp <- pzp %>% 
  mutate (Foal_1_date_temp1 = janitor::convert_to_date(as.numeric(Foal_DOB_P_1yr),
                                                       character_fun = lubridate::dmy),
          Foal_1_temp2 = case_when(
            Foal_DOB_P_1yr == "OUT" ~ "OUT", 
            Foal_DOB_P_1yr == "NP"  ~ "NP",
            Foal_DOB_P_1yr == "NK"  ~ "NK",
            !is.na(Foal_1_date_temp1) ~ "YES")
  ) %>% 
  mutate(Foal_DOB_P_1yr = Foal_1_date_temp1,
         Foal_Status_P_1yr = Foal_1_temp2) %>% 
  dplyr::select (-Foal_1_date_temp1, - Foal_1_temp2,
                 - Booster2_date_temp, - boost_temp_2, -boost_temp_1) %>% 
  mutate (Foal_2_date_temp1 = janitor::convert_to_date(as.numeric(Foal_DOB_P_2yr),
                                                       character_fun = lubridate::dmy),
          Foal_2_temp2 = case_when(
            Foal_DOB_P_2yr == "OUT" ~ "OUT", 
            Foal_DOB_P_2yr == "NP"  ~ "NP",
            Foal_DOB_P_2yr == "NK"  ~ "NK",
            !is.na(Foal_2_date_temp1) ~ "YES") ) %>% 
  mutate(Foal_DOB_P_2yr = Foal_2_date_temp1,
         Foal_Status_P_2yr = Foal_2_temp2) %>% 
  dplyr::select (-Foal_2_date_temp1, - Foal_2_temp2) %>% 
  mutate (Foal_3_date_temp1 = janitor::convert_to_date(as.numeric(Foal_DOB_P_3yr),
                                                       character_fun = lubridate::dmy),
          Foal_3_temp2 = case_when(
            Foal_DOB_P_3yr == "OUT" ~ "OUT", 
            Foal_DOB_P_3yr == "NP"  ~ "NP",
            Foal_DOB_P_3yr == "NK"  ~ "NK",
            !is.na(Foal_3_date_temp1) ~ "YES") ) %>% 
  mutate(Foal_DOB_P_3yr = Foal_3_date_temp1,
         Foal_Status_P_3yr = Foal_3_temp2) %>% 
  dplyr::select (-Foal_3_date_temp1, - Foal_3_temp2) %>% 
  mutate (Foal_4_date_temp1 = janitor::convert_to_date(as.numeric(Foal_DOB_P_4yr),
                                                       character_fun = lubridate::dmy),
          Foal_4_temp2 = case_when(
            Foal_DOB_P_4yr == "OUT" ~ "OUT", 
            Foal_DOB_P_4yr == "NP"  ~ "NP",
            Foal_DOB_P_4yr == "NK"  ~ "NK",
            !is.na(Foal_4_date_temp1) ~ "YES") ) %>% 
  mutate(Foal_DOB_P_4yr = Foal_4_date_temp1,
         Foal_Status_P_4yr = Foal_4_temp2) %>% 
  dplyr::select (-Foal_4_date_temp1, - Foal_4_temp2) %>% 
  mutate (Foal_5_date_temp1 = janitor::convert_to_date(as.numeric(Foal_DOB_P_5yr),
                                                       character_fun = lubridate::dmy),
          Foal_5_temp2 = case_when(
            Foal_DOB_P_5yr == "OUT" ~ "OUT", 
            Foal_DOB_P_5yr == "NP"  ~ "NP",
            Foal_DOB_P_5yr == "NK"  ~ "NK",
            !is.na(Foal_5_date_temp1) ~ "YES") ) %>% 
  mutate(Foal_DOB_P_5yr = Foal_5_date_temp1,
         Foal_Status_P_5yr = Foal_5_temp2) %>% 
  dplyr::select (-Foal_5_date_temp1, - Foal_5_temp2) %>% 
  mutate (Foal_6_date_temp1 = janitor::convert_to_date(as.numeric(Foal_DOB_P_6yr),
                                                       character_fun = lubridate::dmy),
          Foal_6_temp2 = case_when(
            Foal_DOB_P_6yr == "OUT" ~ "OUT", 
            Foal_DOB_P_6yr == "NP"  ~ "NP",
            Foal_DOB_P_6yr == "NK"  ~ "NK",
            !is.na(Foal_6_date_temp1) ~ "YES") ) %>% 
  mutate(Foal_DOB_P_6yr = Foal_6_date_temp1,
         Foal_Status_P_6yr = Foal_6_temp2) %>% 
  dplyr::select (-Foal_6_date_temp1, - Foal_6_temp2) %>% 
  mutate (Foal_7_date_temp1 = janitor::convert_to_date(as.numeric(Foal_DOB_P_7yr),
                                                       character_fun = lubridate::dmy),
          Foal_7_temp2 = case_when(
            Foal_DOB_P_7yr == "OUT" ~ "OUT", 
            Foal_DOB_P_7yr == "NP"  ~ "NP",
            Foal_DOB_P_7yr == "NK"  ~ "NK",
            !is.na(Foal_7_date_temp1) ~ "YES") ) %>% 
  mutate(Foal_DOB_P_7yr = Foal_7_date_temp1,
         Foal_Status_P_7yr = Foal_7_temp2) %>% 
  dplyr::select (-Foal_7_date_temp1, - Foal_7_temp2) %>% 
  mutate (Foal_8_date_temp1 = janitor::convert_to_date(as.numeric(Foal_DOB_P_8yr),
                                                       character_fun = lubridate::dmy),
          Foal_8_temp2 = case_when(
            Foal_DOB_P_8yr == "OUT" ~ "OUT", 
            Foal_DOB_P_8yr == "NP"  ~ "NP",
            Foal_DOB_P_8yr == "NK"  ~ "NK",
            !is.na(Foal_8_date_temp1) ~ "YES") ) %>% 
  mutate(Foal_DOB_P_8yr = Foal_8_date_temp1,
         Foal_Status_P_8yr = Foal_8_temp2) %>% 
  dplyr::select (-Foal_8_date_temp1, - Foal_8_temp2) %>% 
  mutate (Foal_9_date_temp1 = janitor::convert_to_date(as.numeric(Foal_DOB_P_9yr),
                                                       character_fun = lubridate::dmy),
          Foal_9_temp2 = case_when(
            Foal_DOB_P_9yr == "OUT" ~ "OUT", 
            Foal_DOB_P_9yr == "NP"  ~ "NP",
            Foal_DOB_P_9yr == "NK"  ~ "NK",
            !is.na(Foal_9_date_temp1) ~ "YES") ) %>% 
  mutate(Foal_DOB_P_9yr = Foal_9_date_temp1,
         Foal_Status_P_9yr = Foal_9_temp2) %>% 
  dplyr::select (-Foal_9_date_temp1, - Foal_9_temp2) %>% 
  mutate (Foal_10_date_temp1 = janitor::convert_to_date(as.numeric(Foal_DOB_P_10yr),
                                                        character_fun = lubridate::dmy),
          Foal_10_temp2 = case_when(
            Foal_DOB_P_10yr == "OUT" ~ "OUT", 
            Foal_DOB_P_10yr == "NP"  ~ "NP",
            Foal_DOB_P_10yr == "NK"  ~ "NK",
            !is.na(Foal_10_date_temp1) ~ "YES") ) %>% 
  mutate(Foal_DOB_P_10yr = Foal_10_date_temp1,
         Foal_Status_P_10yr = Foal_10_temp2) %>% 
  dplyr::select (-Foal_10_date_temp1, - Foal_10_temp2) %>% 
  mutate (Foal_11_date_temp1 = janitor::convert_to_date(as.numeric(Foal_DOB_P2_1yr),
                                                        character_fun = lubridate::dmy),
          Foal_11_temp2 = case_when(
            Foal_DOB_P2_1yr == "OUT" ~ "OUT", 
            Foal_DOB_P2_1yr == "NP"  ~ "NP",
            Foal_DOB_P2_1yr == "NK"  ~ "NK",
            !is.na(Foal_11_date_temp1) ~ "YES") ) %>% 
  mutate(Foal_DOB_P2_1yr = Foal_11_date_temp1,
         Foal_Status_P2_1yr = Foal_11_temp2) %>% 
  dplyr::select (-Foal_11_date_temp1, - Foal_11_temp2) %>% 
  mutate (Foal_12_date_temp1 = janitor::convert_to_date(as.numeric(Foal_DOB_P2_2yr),
                                                        character_fun = lubridate::dmy),
          Foal_12_temp2 = case_when(
            Foal_DOB_P2_2yr == "OUT" ~ "OUT", 
            Foal_DOB_P2_2yr == "NP"  ~ "NP",
            Foal_DOB_P2_2yr == "NK"  ~ "NK",
            !is.na(Foal_12_date_temp1) ~ "YES") ) %>% 
  mutate(Foal_DOB_P2_2yr = Foal_12_date_temp1,
         Foal_Status_P2_2yr = Foal_12_temp2) %>% 
  dplyr::select (-Foal_12_date_temp1, - Foal_12_temp2)

#
# status:
# YES means mare had a foal in given year
# NP means mare was known to not have foal that year
# NK, OUT and NA all mean mare has to be excluded from that year's ratio or analysis
# because it was either dead, transported or we have no data
#
# removing empty columns ----
pzp <- pzp %>% 
  select (-c(13,14,15,30,33))

# ----
# new columns ----
# first foal after treatment
pzp<- pzp %>% 
  mutate(first_foal_after_treatment = case_when(
    !is.na(Foal_DOB_P_1yr) ~ Foal_DOB_P_1yr,
    !is.na(Foal_DOB_P_2yr) ~ Foal_DOB_P_2yr,
    !is.na(Foal_DOB_P_3yr) ~ Foal_DOB_P_3yr,
    !is.na(Foal_DOB_P_4yr) ~ Foal_DOB_P_4yr,
    !is.na(Foal_DOB_P_5yr) ~ Foal_DOB_P_5yr,
    !is.na(Foal_DOB_P_6yr) ~ Foal_DOB_P_6yr,
    !is.na(Foal_DOB_P_7yr) ~ Foal_DOB_P_7yr,
    !is.na(Foal_DOB_P_8yr) ~ Foal_DOB_P_8yr,
    !is.na(Foal_DOB_P_9yr) ~ Foal_DOB_P_9yr,
    !is.na(Foal_DOB_P_10yr) ~ Foal_DOB_P_10yr))

# Is not relevant for mares who were treated again, so probably not
# a useful column

# days from primer to first foal
 pzp <- pzp %>% 
   mutate(days_primer_to_firstfoal = 
           as.numeric( first_foal_after_treatment - Primer))
 
# fertility / breeding season
 pzp <- pzp %>%  
   mutate(year_primer = as.numeric(format(as.Date (Primer, format="%Y-%m-%d"),"%Y"))) %>% 
   mutate( april_in_year_primer = as.Date(paste (year_primer, "-04-29", sep = "")))
 pzp <- pzp %>% 
   mutate(fertility = case_when(!is.na(DOB_foal_treatment)~  DOB_foal_treatment + 14 ,
                                !is.na(Foal_DOB_P_1yr)  ~ Foal_DOB_P_1yr - 335,
                               # !is.na(DOB_foal_year_before)  ~ DOB_foal_year_before +365,
                                is.na(DOB_foal_treatment)& is.na(Foal_DOB_P_1yr) ~ april_in_year_primer),
          efficiency_booster = fertility - 7,
          efficiency_primer = fertility - 30)%>% 
   select(-april_in_year_primer, -year_primer)
 
# timing of vaccines
 pzp <- pzp %>% mutate(before = case_when (Primer <= efficiency_primer ~ "y",
                                           Primer > efficiency_primer ~ "n" ))%>% 
   mutate(primer_on_time = case_when (!is.na(before) ~ before,
                                               is.na(before) ~ "n")  ) %>% 
   select (-before) %>% 
   mutate(booster_on_time = case_when (Booster1 <= efficiency_booster ~ "y",
                                                Booster1 > efficiency_booster ~ "n" )) %>% 
   mutate(timing = case_when
          (primer_on_time == "y" & booster_on_time == "y"
            ~ "p_b_yes",
            primer_on_time == "y" & booster_on_time =="n"
            ~ "p_yes_b_no",
            primer_on_time == "n" & booster_on_time == "n"
            ~ "p_b_no",
            primer_on_time == "y" & is.na(booster_on_time) 
            ~ "p_yes_nobooster",
            primer_on_time == "n" & is.na(booster_on_time) 
            ~ "p_no_nobooster")
   )

# efficiency in 1 year after treatment
# first year when vaccine should be efficient is calculated differently based on 
# the timing of vaccines.
 pzp <- pzp %>% 
   mutate(efficiency_year1 = case_when(
     (timing == "p_b_yes" & is.na(Foal_DOB_P_1yr)) ~ "yes",
     (timing =="p_b_yes" & !is.na(Foal_DOB_P_1yr)) ~ "no",
     (timing == "p_b_no" & is.na(Foal_DOB_P_2yr)) ~ "yes",
     (timing == "p_b_no" & !is.na(Foal_DOB_P_2yr)) ~ "no",
     (timing == "p_yes_b_no" & is.na(Foal_DOB_P_2yr)) ~ "yes",
     (timing == "p_yes_b_no" & !is.na(Foal_DOB_P_2yr)) ~ "no"
   ))

 write_xlsx (pzp,"przewalski/data/pzp/pzpdata.xlsx")

  