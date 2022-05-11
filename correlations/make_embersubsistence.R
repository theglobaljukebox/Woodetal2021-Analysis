# This script creates the codings for Carol Ember's recoding of Lomax's Subsistience
# Categories from Social Strain
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
})

ea = read.csv('https://raw.githubusercontent.com/D-PLACE/dplace-data/master/datasets/EA/data.csv')
ea_w = pivot_wider(ea[,c("var_id", "code", "soc_id", "year")], 
                   names_from = var_id, 
                   values_from = code, 
                   id_cols = soc_id)

n_codes = 8
coded_data = matrix(NA, nrow = nrow(ea_w), ncol = 9)
colnames(coded_data) = paste0("Code_", 1:9)
coded_data = as.data.frame(coded_data)
coded_data$soc_id = ea_w$soc_id

# Productivity 
# Code = 1
# EA004 < 4 and 
# EA005 < 4 and 
# EA004 + EA005 < 6 
# and the greatest value of (E001, E002, E003) > the greater value of (EA004, EA005)) and 
# Either 
# (EA001 ≥ 4 and EA001 ≥ EA002 and EA003 - EA001 ≤ 1) 
# or 
# (EA001 < 4 and EA001 > EA002 > EA003 or EA001 > EA003 > EA002)

code_1 = ea_w$EA004 < 4 & 
          ea_w$EA005 < 4 &
          (ea_w$EA004 + ea_w$EA005) < 6 & 
          apply(ea_w[,c("EA001", "EA002", "EA003")], 1, max) > apply(ea_w[,c("EA004", "EA005")], 1, max) &
          ((ea_w$EA001 >= 4 & ea_w$EA001 >= ea_w$EA002 & ea_w$EA003 - ea_w$EA001 <= 1) |
          (ea_w$EA001 < 4 & ea_w$EA001 > ea_w$EA002 & ea_w$EA001 > ea_w$EA003))

# add test case
idx = which(coded_data$soc_id == "Aa4")
if(code_1[idx]){
  coded_data$Code_1 = code_1  
} else {
  stop("Code 1 has gone wrong")
}

# Code = 2
# EA004 < 4 and 
# EA005 < 4 and 
# EA004 + EA005 < 6 and 
# the greatest value of (E001, E002, E003) > the greater value of (EA004, EA005)) and 
# does not satisfy the conditions for code = 1

code_2 = ea_w$EA004 < 4 & 
  ea_w$EA005 < 4 &
  ea_w$EA004 + ea_w$EA005 < 6 & 
  apply(ea_w[,c("EA001", "EA002", "EA003")], 1, max) > apply(ea_w[,c("EA004", "EA005")], 1, max) &
  code_1 != TRUE

idx = which(coded_data$soc_id == "Aa2")
if(code_2[idx]){
  coded_data$Code_2 = code_2  
} else {
  stop("Code 2 has gone wrong")
}

# Code = 3
# [EA040=1-3 and 
# EA005>4 and 
# EA028=3] OR
# [EA040-1-2 and 
# EA005>4 and 
# EA003<3 and 
# EA028=4] [this adds horticulturalists with no animals, or only pigs] to this category to distinguish them from horticulturalists in category 6 and category 4.

code_3 = (ea_w$EA040 %in% 1:3 & 
          ea_w$EA005 > 4 & 
          ea_w$EA028 == 3) |
          (ea_w$EA040 %in% 1:2 & 
             ea_w$EA005 > 4 &
             ea_w$EA003 < 3 & 
             ea_w$EA028 == 4)

idx = which(coded_data$soc_id == "Ab16")
if(code_3[idx]){
  coded_data$Code_3 = code_3 
} else {
  stop("Code 3 has gone wrong")
}

# Code = 4 
# EA040 > 2 and 
# EA005 > 4 and 
# Either (EA028 = 5 and EA039 ≠ 3) or
# (EA028 = 3) Or
# (EA028=4 and EA003<3)

code_4 = ea_w$EA040 > 2 &
          ea_w$EA005 > 4 & 
          ((ea_w$EA028 == 5 & ea_w$EA039 !=3) | 
             ea_w$EA028 == 3 | 
             (ea_w$EA028 == 4 & ea_w$EA003 < 3))
  
idx = which(coded_data$soc_id == "Aa6")
if(code_4[idx]){
  coded_data$Code_4 = code_4 
} else {
  stop("Code 4 has gone wrong")
}

# Code = 5
# EA004 > 6 and 
# EA005 < 3 

code_5 = ea_w$EA004 > 5 &
          ea_w$EA005 < 4

idx = which(coded_data$soc_id == "Ab1")
if(code_5[idx]){
  coded_data$Code_5 = code_5
} else {
  stop("Code 5 has gone wrong")
}
# Code = 6
# EA040 > 1 and 
# EA003 > 2 and 
# EA028 = 4 and 
# EA005 > 4

code_6 = ea_w$EA040 > 1 & 
          ea_w$EA003 > 2 &
          ea_w$EA028 == 4 & 
          ea_w$EA005 > 4

idx = which(coded_data$soc_id == "Ie14")
if(code_6[idx]){
  coded_data$Code_6 = code_6
} else {
  stop("Code 6 has gone wrong")
}

# Code = 7
# EA028 = 5 and 
# EA039 = 3 and 
# EA040 = 3 or 7 and 
# EA005 > 4

code_7 = ea_w$EA028 == 5 & 
          ea_w$EA039 == 3 & 
          ea_w$EA040 %in% c(3,7) & 
          ea_w$EA005 > 4

idx = which(coded_data$soc_id == "Ca12")
if(code_7[idx]){
  coded_data$Code_7 = code_7
} else {
  stop("Code 7 has gone wrong")
}

# Code = 8
# EA028 = 6 and 
# EA039 = 3 and 
# EA040 = 3 or 7 and 
# EA005 > 4

code_8 = ea_w$EA028 == 6 & 
  ea_w$EA039 == 3 & 
  ea_w$EA040 %in% c(3,7) & 
  ea_w$EA005 > 4

idx = which(coded_data$soc_id == "Cd12")
if(code_8[idx]){
  coded_data$Code_8 = code_8
} else {
  stop("Code 8 has gone wrong")
}

coded_data$categories = rowSums(coded_data[,1:n_codes])

ea_variables = data.frame(
  EA001 = ea_w$EA001,
  EA002 = ea_w$EA002,
  EA003 = ea_w$EA003,
  EA004 = ea_w$EA004,
  EA005 = ea_w$EA005,
  EA040 = ea_w$EA040,
  EA028 = ea_w$EA028,
  EA039 = ea_w$EA039
)

coded_data = cbind(coded_data, ea_variables)

write.csv(coded_data, "data/ember_subsistence.csv")

### Get data
subsistence_df = coded_data[coded_data$categories == 1 & 
                              !is.na(coded_data$categories),]

subsistence = ifelse(subsistence_df$Code_1 == TRUE, 1,
                     ifelse(subsistence_df$Code_2 == TRUE, 2,
                            ifelse(subsistence_df$Code_3 == TRUE, 3,
                                   ifelse(subsistence_df$Code_4 == TRUE, 4,
                                          ifelse(subsistence_df$Code_5 == TRUE, 5,
                                                 ifelse(subsistence_df$Code_6 == TRUE, 6,
                                                        ifelse(subsistence_df$Code_7 == TRUE, 7,
                                                               ifelse(subsistence_df$Code_8 == TRUE, 8, 
                                                                      NA))))))))

out = data.frame(society = subsistence_df$soc_id, 
                 subsistence = subsistence)

write.csv(out, "data/embersubsistence_data.csv")
