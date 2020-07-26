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

amova(indianCanto$samples, indianCanto$distances, indianCanto$structures)