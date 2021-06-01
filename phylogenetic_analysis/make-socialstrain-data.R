#### This script will get the data for phylogenetic comparisons and maps
#### It currently requires access to the google sheets file containing 
#### social layering data. 

library(googlesheets4)
library(dplyr)

# get stratification data
stratification = read_sheet("https://docs.google.com/spreadsheets/d/1M8gDb9GxWQKEVcUcPbKOgNu0OizoCGC8ht_gBBPon8s/edit#gid=1032523489")
dplace_codes = read.csv('https://raw.githubusercontent.com/D-PLACE/dplace-data/master/datasets/EA/societies.csv')

stratification = left_join(stratification, dplace_codes, c("soc_id" = "id"))

# get cantometrics data
cantometrics_modal = read.csv("output/modal_profiles.csv")
cantometrics_modal$soc_id = as.numeric(cantometrics_modal$soc_id)

cantometrics_metadata = read.csv('https://raw.githubusercontent.com/theglobaljukebox/cantometrics/main/raw/societies.csv?token=AAM3DDZI6G63T7MYNLY3BZ3AW2V7W')
cantometrics = left_join(cantometrics_modal, cantometrics_metadata, by = c("soc_id" = "society_id")) 

cantometrics = cantometrics %>% 
  dplyr::select(soc_id, line_23, line_4, line_22, Society_latitude, Society_longitude, Glottocode, Language_family)

ea_cantometrics = full_join(cantometrics, stratification, c("Glottocode" = "glottocode"),  suffix = c(".canto", ".strat"))

# Only keep columns we care about
keep_cols = c("soc_id.strat", "xd_id.x", "soc_id.canto", "pref_name_for_society.x", "Glottocode", 
              "SocialFactors_V33_code", "line_23", "line_4", "line_22",
              "Society_latitude", "Society_longitude", "Language_family")

ea_cantometrics = ea_cantometrics[,keep_cols]

#### Make Colours ####
values2col = function(x, col = c("#5E9AC4", "#88C1CA", "#F6C3C3", "#F1635B", "#C52B2F")){
  require(RColorBrewer)
  x = as.character(x)
  values = sort(unique(x))
  matcher = data.frame(values = values,
                       colours = col[1:length(values)])
  
  dplyr::left_join(data.frame(x), matcher, c("x" = "values"))[,2]
}

# line 23
# Reverse default colours for line_23 because Extreme Embellishment is a low number. 
ea_cantometrics$line23_col = values2col(ea_cantometrics$line_23, col = rev(c("#5E9AC4", "#88C1CA", "#F6C3C3", "#F1635B", "#C52B2F")))
# stratification
ea_cantometrics$stratification_col = values2col(ea_cantometrics$SocialFactors_V33_code)
# line 4
ea_cantometrics$line4_col = values2col(ea_cantometrics$line_4)
# line 22
ea_cantometrics$line22_col = values2col(ea_cantometrics$line_22)

sum(!is.na(ea_cantometrics$SocialFactors_V33_code))

write.csv(ea_cantometrics, file = "data/socialfactors_cantomtericsmodalprofiles.csv")
