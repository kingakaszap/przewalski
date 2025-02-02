library(readxl)
library(tidyverse)


pzp_new <- read_excel("przewalski/data/pzp_new.xlsx")

transport <- read_excel("przewalski/data/transports.xlsx")

names <- pzp_new$Name

transport_pzp <- filter(transport, 
(name %in% names))



