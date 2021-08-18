# Line 10 models
suppressMessages(library(tidyr))
suppressMessages(library(lmerTest))
suppressMessages(library(dplyr))
suppressMessages(library(spaMM))
suppressMessages(library(ape))
suppressMessages(library(geiger))
suppressMessages(library(phylolm))
library(ggplot2)
source("correlations/helper.R")

model_df = read.csv(file = "data/cantometrics_ethnographicatlas.csv")

# remove all language families with <5 languages
tt = table(model_df$Language_family)
tt_idx = tt >= 2
big_languagefamilies = model_df[model_df$Language_family %in% names(tt)[tt_idx],]

# N language families vs N Divisions
# n_distinct(big_languagefamilies$Language_family)
# n_distinct(big_languagefamilies$Division)

data.10 = big_languagefamilies %>% 
  drop_na(line_10, std_subsistence)

#### Continuous + RE ####

fit.10.1.3 = lm(line_10 ~ std_subsistence, data = data.10)

bivariate_line = c(
  "line_10 ~ std_subsistence",
  round(coef(fit.10.1.3),2), 
  summary(fit.10.1.3)$coefficients[,4],
  round(AIC(fit.10.1.3), 2))

### More complex models
## Linguistic model
tree = read.tree('data/super_tree.nwk')
data.10LF = data.10[!duplicated(data.10$Glottocode),]
rownames(data.10LF) = data.10LF$Glottocode
pruned = treedata(phy = tree, data = data.10LF)
pruned_data = data.frame(pruned$data)
pruned_data$line_10 = as.numeric(pruned_data$line_10)
pruned_data$std_subsistence = as.numeric(pruned_data$std_subsistence)
pruned_data$Society_latitude = as.numeric(pruned_data$Society_latitude)
pruned_data$Society_longitude = as.numeric(pruned_data$Society_longitude)
pruned_tree = pruned$phy

# Standardize branch lengths
pruned_tree$edge.length = pruned_tree$edge.length / max(pruned_tree$edge.length)

phylo_model = phylolm(formula = line_10 ~ std_subsistence, 
                      data = pruned_data, 
                      phy = pruned_tree, 
                      model = "lambda")
phylo_summary = summary(phylo_model)

## Spatial model
spatial_model = fitme(
  line_10 ~ std_subsistence + Matern(1 | Society_longitude + Society_latitude),
  data = pruned_data,
  fixed = list(nu = 0.5), 
  method="REML")

spatial_summary = summary(spatial_model)

sp_aic = AIC(spatial_model)

spatial_line = c(
  "line_10 ~ std_subsistence",
  round(fixef(spatial_model),2), 
  round(pt(spatial_summary$beta_table[,3], nrow(data.10) - 5, lower.tail = FALSE)),
  sp_aic[1])
names(spatial_line) = c("model", "Intercept", "Beta", 
                        "intercept-p", "beta-p",
                        "AIC")

phylo_line = c(
  "line_10 ~ std_subsistence",
  round(phylo_summary$coefficients[,1],2), 
  phylo_summary$coefficients[,4],
  AIC(phylo_model)
)
names(phylo_line) = c("model", "Intercept", "Beta", 
                      "intercept-p", "beta-p",
                      "AIC")

write.csv(rbind(bivariate_line, spatial_line, phylo_line), file = "correlations/results/complex_line10.csv")

# plot of effect
data.10$fit <- predict(fit.10.3.3)   
# Add model fits to dataframe

# intercept_data = data.frame(std_subsistence = 0,
#                           Division = unique(data.10$Division))
# 
# intercept_data$intercept = predict(fit.10.3.3, intercept_data)

new_data = data.frame(std_subsistence = 
                        seq(from = 0, to = 1, 
                            length.out = 100),
                      Division = "East Africa")

new_data$fit = predict(fit.10.1.3, new_data)

plot_1 = ggplot(data.10, aes(y = line_10, 
                              x = std_subsistence)) + 
  # geom_point(aes(y=intercept, x = 0, group = Division), 
  #            size = 3, alpha = 0.5, pch = "_", 
  #            data = intercept_data, 
  #            col = "#CF4520") + 
  ylim(c(0,1)) +
  geom_line(aes(y = fit), data = new_data) +
  geom_jitter(alpha = 0.3, width = 0.02, height = 0.02) + 
  theme(legend.position = "none") + 
  xlab("Subsistence: Maximum standardized") + 
  ylab("CV10: Maximum standardized") + 
  ggtitle("Text Repetition (CV10) vs Subsistence")

ggsave(filename = 'figs/line10_modelplot.png', plot = plot_1)

