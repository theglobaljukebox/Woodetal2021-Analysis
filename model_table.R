
# make data
system("Rscript make_modeldata.R")

## Run models
system("Rscript line3_models.R")
system("Rscript line10_models.R")
system("Rscript line21_models.R")
system("Rscript line23_models.R")
system("Rscript line37_models.R")
system("Rscript pca_models.R")


## Get model output
line3  = read.csv('phylogenetic_analysis/line3.csv')
line10 = read.csv('phylogenetic_analysis/line10.csv')
line21 = read.csv('phylogenetic_analysis/line21.csv')
line23 = read.csv('phylogenetic_analysis/line23.csv')
line37 = read.csv('phylogenetic_analysis/line37.csv')
pca    = read.csv('phylogenetic_analysis/pca.csv')

# Make table
out = cbind(line3, line10[,2], line21[,2], line23[,2], line37[,2], 
            pca[,2])

write.csv(t(out), file = "phylogenetic_analysis/model_table.csv", row.names = FALSE)
