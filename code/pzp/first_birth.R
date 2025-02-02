# elso ellos kancak vs mindenki mas csiko tulelesi rata


library(tidyverse)

lista <- read_excel("23_nyar/phorse/nagylista_szetszedve.xlsx")

lista <- lista %>% 
  filter(Place_of_birth == "Hortobagy(HU)", !(Name_mother == "NA")) %>% 
  filter(! grepl("UKFEM", Name_mother)) %>% 
  rename("id"="N") 

# nem ulla??

lista <- lista %>% 
  group_by (Name_mother) %>% 
  mutate(first_birth_mother = min(year_of_birth)) %>% 
  ungroup()

lista <-lista %>% 
  mutate_if(is.POSIXct, as.Date) %>% 
  group_by(year_of_birth) %>% 
  mutate( diff = year_of_birth - first_birth_mother) %>% 
  ungroup() %>% 
  mutate(birth_group = case_when(
    diff >0 ~ "notfirst",
    diff == "0" ~ "first"
  ))

 lista1 <- lista %>% 
  mutate(lifetime = year_of_death - year_of_birth,
 lifetime_days = as.numeric(Date_of_death - Date_of_birth)) %>% 
   mutate (foal_survival_1month = dplyr::case_when
           (lifetime_days <= 31 ~ "dead",
            lifetime_days > 31 ~ "notdead",
             is.na(lifetime_days) ~ "notdead"))


summary_try_1 <- lista1 %>% 
  group_by(birth_group, foal_survival_1month) %>% 
  summarise(total = length(id))

summary_try_2 <- lista1 %>% 
  group_by(year_of_birth,birth_group, foal_survival_1month) %>% 
  summarise(total = length(id))

summary_try_3 <- lista1 %>% 
  group_by(birth_group, year_of_birth) %>% 
  summarise(dead =  (nrow(filter( foal_survival_1month == "dead"))),
                                notdead = (nrow(filter( foal_survival_1month == "notdead"))
                                ))


summary_forplot <- lista1 %>% 
  group_by(year_of_birth, birth_group, ) %>% 
  summarise(percentage_dead = dead/(notdead))


# csiko aki kevesebb mint 1 honapot elt mint dead
#-----
# ellesek majus elott ----

before_may <- (filter(lista, month_of_birth %in% c(1,2,3,4) )) 
nrow(before_may)
before_april <- (filter(lista, month_of_birth %in% c(1,2,3) )) 
nrow(before_april)
before_april_15<- lista %>% 
  filter(month_of_birth %in% c(1,2,3) | (month_of_birth == "4" & day_of_birth < 15))

lista_filtered <- lista %>% 
  select (month_of_birth, N, Name)
lista_filtered <- na.omit(lista_filtered)
nrow(before_april_15) / (nrow(before_april_15) + nrow(lista_filtered))
# ellesek 13 szalaeka majus elott van
# kevesebb mint ? % van aprilis elott viszont
# es kevesebb mint 3%-a aprilis 15 elott
