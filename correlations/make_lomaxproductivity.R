# This script creates the codings for Carol Ember's recoding of Lomax's Subsistience
# Categories from Social Strain

library(dplyr)
library(tidyr)

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

table(code_1)

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

table(code_2)

idx = which(coded_data$soc_id == "Aa2")
if(code_2[idx]){
  coded_data$Code_2 = code_2  
} else {
  stop("Code 2 has gone wrong")
}

# Code = 3 (moved to the bottom)

# Code = 4 
# EA040 > 1 and 
# EA005 > 0 and
# Either 
# (EA028 = 4, 5, or 6 and EA039 ≠ 3)
# or
# (EA028 = 3). 


code_4 = ea_w$EA040 > 1 &
          ea_w$EA005 > 0 & 
          ((ea_w$EA028 %in% c(4,5,6) & ea_w$EA039 != 3) |
          (ea_w$EA028 == 3))
  
table(code_4)

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
          ea_w$EA005 < 3

idx = which(coded_data$soc_id == "Ca43")
if(code_5[idx]){
  coded_data$Code_5 = code_5
} else {
  stop("Code 5 has gone wrong")
}

# Code = 6
# EA040 > 1 and 
# EA003 > 2 and 
# EA028 = 4

code_6 = ea_w$EA040 > 1 & 
          ea_w$EA003 > 2 &
          ea_w$EA028 == 4

table(code_6)

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
# EA005 > (EA004)/2

code_7 = ea_w$EA028 == 5 & 
          ea_w$EA039 == 3 & 
          ea_w$EA040 %in% c(3, 7) & 
          ea_w$EA005 > (ea_w$EA004 / 2)

table(code_7)

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

code_8 = ea_w$EA028 == 6 & 
          ea_w$EA039 == 3 & 
          ea_w$EA040 %in% c(3, 7) 

table(code_8)

idx = which(coded_data$soc_id == "Cd10")
if(code_8[idx]){
  coded_data$Code_8 = code_8
} else {
  stop("Code 8 has gone wrong")
}

# Code = 3
# Does not satisfy conditions for 5_1, 5_2, 5_4, 5_5, 5_6, 5_7, or 5_8 
# (i.e. does not fit in any other category)
code_3 = code_1 == FALSE & 
          code_2 == FALSE & 
          code_4 == FALSE & 
          code_5 == FALSE & 
          code_6 == FALSE & 
          code_7 == FALSE & 
          code_8 == FALSE

table(code_3)

idx = which(coded_data$soc_id == "Aa3")
if(code_3[idx]){
  coded_data$Code_3 = code_3 
} else {
  stop("Code 3 has gone wrong")
}

coded_data$categories = rowSums(coded_data[,1:8])

table(coded_data$categories)

write.csv(coded_data, "data/lomax_subsistence.csv")
