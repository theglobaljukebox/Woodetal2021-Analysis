# This script recreates the Social Layering variable from Lomax

library(dplyr)
library(tidyr)
library(vegan)
library(ggplot2)

ea = read.csv('https://raw.githubusercontent.com/D-PLACE/dplace-data/master/datasets/EA/data.csv')
ea_w = pivot_wider(ea[,c("var_id", "code", "soc_id", "year")], 
                   names_from = var_id, 
                   values_from = code, 
                   id_cols = soc_id)


# Social layering
# Class (EA066)
# Changes: 1=0; 2=1; 4=2; 3=3; 5=4
class = ifelse(ea_w$EA066 == 1, 0,
               ifelse(ea_w$EA066 == 2, 1,
                      ifelse(ea_w$EA066 == 4, 2,
                             ifelse(ea_w$EA066 == 3, 3,
                                    ifelse(ea_w$EA066 == 5, 4, NA)))))

table(class)

# Caste (EA068)
# Changes: 1=0; 2=1; 3=2; 4=3
caste = ea_w$EA068 - 1

table(caste)

# Slavery (EA071)
# Changes: 1=0; 2=1; 3 or 4 =2
slavery = ifelse(ea_w$EA071 == 1, 0,
                 ifelse(ea_w$EA071 == 2, 1, 
                        ifelse(ea_w$EA071 %in% 3:4, 2, NA)))

table(slavery)
# function for rescaling variables between 0 & 1
range01 <- function(x){(x-min(x, na.rm = TRUE))/(max(x, na.rm = TRUE)-min(x, na.rm = TRUE))}

class_scaled = range01(class)
caste_scaled = range01(caste)
slavery_scaled = range01(slavery)

sociallayering_add = class_scaled + caste_scaled + slavery_scaled

pca_df = cbind(class_scaled, caste_scaled, slavery_scaled)
pca_df = pca_df[complete.cases(pca_df),]
pca_out = princomp(pca_df)

summary(pca_out)
pca_out$loadings

sociallayering_pca = pca_out$scores

out = data.frame(soc_id = ea_w$soc_id,
                  class = class, caste = caste, slavery = slavery,
                 class_scaled = class_scaled, 
                 caste_scaled = caste_scaled, 
                 slavery_scaled = slavery_scaled,
                 SocialLayering_additive = sociallayering_add)

write.csv(out, "data/social_layering.csv")


# plot_df = data.frame(value = c(class_scaled, caste_scaled, slavery_scaled, social_layering),
#                      type = rep(c("Class", "Caste", "Slavery", "SocialLayering"), each = nrow(ea_w)))
# 
# ggplot(plot_df, aes(x = value)) + geom_histogram() + 
#   facet_wrap(~type)
