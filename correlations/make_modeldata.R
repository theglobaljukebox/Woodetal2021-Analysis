suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(googlesheets4)
  library(assertthat)
  library(psych)
})

source("correlations/helper.R")

#### Cantometrics ####
cantometrics_modal = read.csv("output/modal_profiles.csv", na.strings = "")
cantometrics_modal = cantometrics_modal[!is.na(cantometrics_modal$soc_id),]
cantometrics_modal$soc_id = as.numeric(cantometrics_modal$soc_id)

cantometrics_metadata = read_sheet("https://docs.google.com/spreadsheets/d/1tb3Nip43e4LaJbglaXzcCTP2CiMyrgwIsU2egk3tfNM/edit#gid=1190601304")
cantometrics_metadata = read.csv("https://github.com/theglobaljukebox/cantometrics/blob/main/cldf/societies.csv")

cantometrics_metadata$EA_code = cantometrics_metadata$default_DPL_soc_id
cantometrics = left_join(cantometrics_modal, cantometrics_metadata, 
                         by = c("soc_id" = "society_id"))

cantometrics = cantometrics %>% 
  dplyr::select(soc_id, EA_code, line_7, line_10, line_21, line_23, line_37, 
                Society_latitude, Society_longitude, GlottoID,
                FamilyLevGlottocode, Region, Division)

x = assert_that(all(table(cantometrics$soc_id) == 1), 
            msg = "All societies should only occur once")

#### Ethnographic Atlas ####
ea_societies = read.csv('https://raw.githubusercontent.com/D-PLACE/dplace-data/master/datasets/EA/societies.csv')

subsistence = read.csv('data/embersubsistence_data.csv')
social_layering = read.csv('data/social_layering.csv')

ea_data = read.csv("https://raw.githubusercontent.com/D-PLACE/dplace-data/master/datasets/EA/data.csv")
ea_data = ea_data %>% 
  filter(var_id == "EA033" | var_id == "EA031") %>%
  dplyr::select(soc_id, var_id, code)

ea_w = pivot_wider(ea_data, names_from = var_id, values_from = code)

social_data = full_join(subsistence, social_layering, by = c("society" = "soc_id")) %>% 
  left_join(., ea_w, by = c("society" = "soc_id"))

# Number of data points shouldn't change
x = assert_that(nrow(social_data) == 1291)
# All societies occur once
x = assert_that(sum(table(social_data$society) > 1) == 0)

ea_metadata = left_join(social_data, ea_societies, by = c("society" = "id"))

x = assert_that(all(table(ea_metadata$society) == 1), 
            msg = "All societies should only occur once")


#### Join Cantometrics and Ethnographic Atlas ####
cantometrics = cantometrics[!duplicated(cantometrics$EA_code),]
model_data = left_join(cantometrics, 
                       ea_metadata, by = c("EA_code" = "society"))

x = assert_that(all(table(model_data$society) == 1), 
            msg = "All societies should only occur once")

# standardize musical variables convert to 0-1 scale
model_data$line_7 = musical_conversion(model_data$line_7, c(1, 4, 7, 10, 13))
model_data$line_10 = musical_conversion(model_data$line_10, c(1, 4, 7, 10, 13))
model_data$line_21 = musical_conversion(model_data$line_21, c(1, 4, 7, 10, 13))
model_data$line_23 = musical_conversion(model_data$line_23, c(1, 4, 7, 10, 13))
model_data$line_37 = musical_conversion(model_data$line_37, c(1, 4, 7, 10, 13))

# Reverse code embellishment
model_data$line_23 = 1 - model_data$line_23
# Reverse code enunciation
model_data$line_37 = 1 - model_data$line_37

## Standardize EA variables
model_data$std_subsistence = model_data$subsistence / max(model_data$subsistence, na.rm = TRUE)
model_data$std_caste = model_data$caste / max(model_data$caste, na.rm = TRUE)
model_data$std_slavery = model_data$slavery / max(model_data$slavery, na.rm = TRUE)
model_data$std_class = model_data$class / max(model_data$class, na.rm = TRUE)
model_data$std_EA033 = model_data$EA033 / max(model_data$EA033, na.rm = TRUE)
model_data$std_EA031 = model_data$EA031 / max(model_data$EA031, na.rm = TRUE)

#### PCA ####
## Make PCA variables
pca_data = model_data %>% 
  dplyr::select(soc_id, line_7, line_10, line_21, line_23, line_37,
                std_subsistence, std_caste, std_slavery, std_class,
                std_EA033, std_EA031) %>% 
  filter(complete.cases(.))


x = assert_that(sum(is.na(pca_data[,2:6])) == 0)

jpeg('figs/musical_scree.jpeg')
psych::scree(pca_data[,c("line_7", "line_10", "line_21", 
                         "line_23", "line_37")], factors = FALSE)
dev.off()

musical_pca = psych::principal(pca_data[,c("line_7", "line_10", "line_21", 
                                           "line_23", "line_37")], 
                               rotate="varimax", 
                               scores = TRUE, 
                               nfactors = 1)
pca_data$musical_pc1 = musical_pca$scores[,1]

jpeg('figs/social_scree.jpeg')
psych::scree(pca_data[,c("std_subsistence", "std_caste", 
                         "std_slavery", "std_class",
                         "std_EA033", "std_EA031")], factors = FALSE)
dev.off()

social_pca = psych::principal(pca_data[,c("std_subsistence", "std_caste", 
                                          "std_slavery", "std_class",
                                          "std_EA033", "std_EA031")], 
                              rotate = "varimax", 
                              scores = TRUE, 
                              nfactors = 1)
pca_data$social_pc1 = social_pca$scores[,1]

x = assert_that(all(table(pca_data$soc_id) == 1))
x = assert_that(all(table(pca_data$soc_id) == 1))

model_data = left_join(model_data, pca_data[,c("soc_id", "musical_pc1", "social_pc1")], by = "soc_id") 

x = assert_that(all(table(model_data$soc_id) == 1), 
            msg = "All societies should only occur once")

write.csv(model_data, "data/cantometrics_ethnographicatlas.csv")
