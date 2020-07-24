library('vegan')
library('dplyr2')
library('ggplot2')
library('textshape')
setwd('~/Documents/Work/Global Jukebox/')

# load csv file
indian_dataset <- read.csv('./data/indian_music_full.csv', header = TRUE)

# drop unnecessary columns
indian_subset = subset(indian_dataset, select=-c(lat, lng, culture, region))

# set rownames
column_to_rownames(indian_subset, 'canto_coding_id') 

# distance matrix
indian.dist <- dist(indian_subset, method="euclidean")

#mds, convert to dataframe 
fit <- data.frame(cmdscale(indian.dist))

culture <- indian_dataset$culture
region <- indian_dataset$region

# plot
ggplot(fit, aes(x=X1, y=X2)) 
+ geom_label(aes(fill=region, label=culture)) 
+ ggtitle("Two-dimensional Multi-Dimensional Scaling Plot of Indian Dataset") 
+ xlab("Dimension 2") 
+ ylab("Dimension 1")
