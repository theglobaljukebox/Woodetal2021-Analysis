## Get model output
line7  = read.csv('correlations/results/complex_line7.csv')
line10 = read.csv('correlations/results/complex_line10.csv')
line21 = read.csv('correlations/results/complex_line21.csv')
line23 = read.csv('correlations/results/complex_line23.csv')
line37 = read.csv('correlations/results/complex_line37.csv')
pca    = read.csv('correlations/results/complex_pca.csv')




# Make table
out = cbind(line7[,2], line10[,2], line21[,2], line23[,2], line37[,2], 
            pca[,2])

write.csv(t(out), file = "correlations/results/model_table.csv", row.names = FALSE)
