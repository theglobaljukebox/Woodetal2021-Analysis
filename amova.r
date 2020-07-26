library('textshape')
library('ade4')
setwd('~/Documents/Work/Global Jukebox/')

indianCanto <- list("samples", "distances", "structures")
df <- read.csv('./data/samples.csv')
indianCanto$samples = subset(df, select = -c(X))
indianCanto$samples <- column_to_rownames(indianCanto$samples, "canto_coding_id")

df <- read.csv('./data/indian_no_singles.csv')
df <- subset(df, select = -c(X, Number.of.Samples, region, division, subregion, area_kingdom, culture, lat, lng))
df <- column_to_rownames(df, "canto_coding_id")
indianCanto$distances <- dist(df)

df <- read.csv('./data/structure.csv')
df = subset(df, select=-c(X))
factors <- df[['region']]
indianCanto$structures <- data.frame(region = as.factor(factors))

indianCanto$amova <- amova(indianCanto$samples, indianCanto$distances, indianCanto$structures)


globalCanto <- list("samples", "distances", "structures")
df <- read.csv('./data/global_samples.csv')
globalCanto$samples = subset(df, select = -c(X))
globalCanto$samples <- column_to_rownames(globalCanto$samples, "canto_coding_id")

df <- read.csv('./data/global_no_singles.csv')
df <- subset(df, select = -c(X, Culture, C_cid, orv_1,orv_2,ensemble_value_id,ensemble_value_label,instrument_value_id,instrument_value_label,Number.of.Samples))
df <- column_to_rownames(df, "canto_coding_id")
globalCanto$distances <- dist(df)

df <- read.csv('./data/global_structure.csv')
df = subset(df, select=-c(X))
factors <- df[['region']]
globalCanto$structures <- data.frame(region = as.factor(factors))

globalCanto$amova <- amova(globalCanto$samples, globalCanto$distances, globalCanto$structures)