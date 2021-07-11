library(dplyr)
library(googlesheets4)

#### Cantometrics ####
cantometrics_modal = read.csv("output/modal_profiles.csv")
cantometrics_modal$soc_id = as.numeric(cantometrics_modal$soc_id)

cantometrics_metadata = read_sheet("https://docs.google.com/spreadsheets/d/1tb3Nip43e4LaJbglaXzcCTP2CiMyrgwIsU2egk3tfNM/edit#gid=1190601304")
cantometrics = left_join(cantometrics_modal, cantometrics_metadata, by = c("soc_id" = "society_id")) 

cantometrics = cantometrics %>% 
  dplyr::select(soc_id, line_3, line_10, line_21, line_23, line_37, 
                Society_latitude, Society_longitude, Glottocode, 
                Language_family, Region, Division)

#### Ethnographic Atlas ####
ea_societies = read.csv('https://raw.githubusercontent.com/D-PLACE/dplace-data/master/datasets/EA/societies.csv')
subsistence = read.csv('data/embersubsistence_data.csv')

ea_data = left_join(subsistence, ea_societies, by = c("society" = "id"))

model_data = left_join(cantometrics, 
                       ea_data, by = c("Glottocode" = "glottocode"))
