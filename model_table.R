## Get model output
line7  = read.csv('phylogenetic_analysis/line7.csv')
line10 = read.csv('phylogenetic_analysis/line10.csv')
line21 = read.csv('phylogenetic_analysis/line21.csv')
line23 = read.csv('phylogenetic_analysis/line23.csv')
line37 = read.csv('phylogenetic_analysis/line37.csv')
pca    = read.csv('phylogenetic_analysis/pca.csv')

# Make table
out = cbind(line7[,2], line10[,2], line21[,2], line23[,2], line37[,2], 
            pca[,2])

write.csv(t(out), file = "phylogenetic_analysis/model_table.csv", row.names = FALSE)
