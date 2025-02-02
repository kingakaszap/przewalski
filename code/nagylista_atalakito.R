# nagylistabol datumokat szetszedni

library(tidyverse)
library(janitor)
library(lubridate)
library(writexl)

nagylista <- read_excel("C:/Users/Kinga/Downloads/Przewalski horses Hortobágy 2023.07.10 (1).xls")


nagylista<- nagylista %>% 
  mutate(Date_of_birth =  janitor::convert_to_date(as.numeric(nagylista$Date_of_birth), character_fun = lubridate::dmy),
         Date_of_death =  janitor::convert_to_date(as.numeric(nagylista$Date_of_death), character_fun = lubridate::dmy)
  )

nagylista<- nagylista %>%  
  mutate(year_of_birth = format(as.Date (Date_of_birth, format="%Y-%m-%d"),"%Y"),
         month_of_birth = format(as.Date (Date_of_birth, format = "%Y-%m-%d"), "%m"),
         day_of_birth = format (as.Date (Date_of_birth, format = "%Y-%m-%d"), "%d"),
         year_of_death = format(as.Date (Date_of_death, format="%Y-%m-%d"),"%Y"),
         month_of_death = format(as.Date (Date_of_death, format = "%Y-%m-%d"), "%m"),
         day_of_death = format (as.Date (Date_of_death, format = "%Y-%m-%d"), "%d")
  )

nagylista <- nagylista %>% 
  mutate_at(c("year_of_birth",          
 "month_of_birth",           
 "day_of_birth",           
 "year_of_death",           
 "month_of_death",           
 "day_of_death"), as.numeric)

write_xlsx(nagylista,"C:\\Users\\Kinga\\Documents\\R_ideiglenes\\nagylista_szetszedve.xlsx")
lista <- read_excel("C:\\Users\\Kinga\\Documents\\R_ideiglenes\\nagylista_szetszedve.xlsx")
