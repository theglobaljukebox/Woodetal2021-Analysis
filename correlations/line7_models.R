# Line 7 models
suppressMessages(library(lmerTest))
suppressMessages(library(tidyr))
suppressMessages(library(spaMM))
suppressMessages(library(phylolm))
suppressMessages(library(ape))
suppressMessages(library(geiger))
library(ggplot2)

source("correlations/helper.R")

model_df = read.csv(file = "data/cantometrics_ethnographicatlas.csv")

# remove all language families with <5 languages
tt = table(model_df$Language_family)
tt_idx = tt >= 2
big_languagefamilies = model_df[model_df$Language_family %in% names(tt)[tt_idx],]

data.7 = big_languagefamilies %>% 
  drop_na(line_7, std_EA033)

#### Models ####

fit.4.0 = lm(line_7 ~ std_EA033, data = data.7)

bivariate_line = c(
  "line_7 ~ std_EA033",
  round(coef(fit.4.0),2), 
  summary(fit.4.0)$coefficients[,4],
  round(AIC(fit.4.0), 2))

### More complex models
## Linguistic model
tree = read.tree('data/super_tree.nwk')
data.7LF = data.7[!duplicated(data.7$Glottocode),]
rownames(data.7LF) = data.7LF$Glottocode
pruned = treedata(phy = tree, data = data.7LF)
pruned_data = data.frame(pruned$data)
pruned_data$line_7 = as.numeric(pruned_data$line_7)
pruned_data$std_EA033 = as.numeric(pruned_data$std_EA033)
pruned_data$Society_latitude = as.numeric(pruned_data$Society_latitude)
pruned_data$Society_longitude = as.numeric(pruned_data$Society_longitude)
pruned_tree = pruned$phy

# Standardize branch lengths
pruned_tree$edge.length = pruned_tree$edge.length / max(pruned_tree$edge.length)

phylo_model = phylolm(formula = line_7 ~ std_EA033, 
                      data = pruned_data, 
                      phy = pruned_tree, 
                      model = "lambda")
phylo_summary = summary(phylo_model)

## Spatial model
spatial_model = fitme(
  line_7 ~ std_EA033 + Matern(1 | Society_longitude + Society_latitude),
  data = pruned_data,
  fixed = list(nu = 0.5), 
  method="REML")

spatial_summary = summary(spatial_model)

sp_aic = AIC(spatial_model)

spatial_line = c(
  "line_7 ~ std_EA033",
  round(fixef(spatial_model),2), 
  round(pt(spatial_summary$beta_table[,3], nrow(data.7) - 5, lower.tail = FALSE)),
  sp_aic[1])
names(spatial_line) = c("model", "Intercept", "Beta", 
                        "intercept-p", "beta-p",
                        "AIC")

phylo_line = c(
  "line_7 ~ std_EA033",
  round(phylo_summary$coefficients[,1],2), 
  phylo_summary$coefficients[,4],
  AIC(phylo_model)
)
names(phylo_line) = c("model", "Intercept", "Beta", 
                      "intercept-p", "beta-p",
                      "AIC")


write.csv(rbind(bivariate_line, spatial_line, phylo_line), file = "correlations/results/complex_line7.csv")


# plot of effect
data.7$fit <- predict(fit.4.2)   
# Add model fits to dataframe

intercept_data = data.frame(std_EA033 = 0,
                            Division = unique(data.7$Division))

intercept_data$intercept = predict(fit.4.2, intercept_data)

new_data = data.frame(std_EA033 = 
                        seq(from = 0, to = 1, 
                            length.out = 100),
                      Division = "East Africa")

new_data$fit = predict(fit.4.2, new_data)

plot_1 = ggplot(data.7, aes(y = line_7, 
                             x = std_EA033)) + 
  geom_point(aes(y=intercept, x = 0, group = Division), 
             size=3, alpha = 0.5, pch = "_", 
             data = intercept_data,
             col = "#CF4520") + 
  ylim(c(0,1)) +
  geom_line(aes(y = fit), data = new_data) +
  geom_jitter(alpha = 0.3, width = 0.02, height = 0.02) + 
  theme(legend.position = "none") + 
  xlab("EA033: Maximum standardized") + 
  ylab("CV7: Maximum standardized") + 
  ggtitle("Musical organization of the orchestra (CV7) vs Jurisdictional hierarchy (EA033)",
          "Division:East Africa regression line")

ggsave(filename = 'figs/line7_modelplot.png', plot = plot_1)

