## Get model output
line7  = read.csv('correlations/results/line7.csv')
line10 = read.csv('correlations/results/line10.csv')
line21 = read.csv('correlations/results/line21.csv')
line23 = read.csv('correlations/results/line23.csv')
line37 = read.csv('correlations/results/line37.csv')
pca    = read.csv('correlations/results/pca.csv')

# Make table
out = cbind(line7[,2], line10[,2], line21[,2], line23[,2], line37[,2], 
            pca[,2])

write.csv(t(out), file = "correlations/results/model_table.csv", row.names = FALSE)
