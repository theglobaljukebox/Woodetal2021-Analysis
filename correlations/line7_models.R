# Line 7 models
suppressPackageStartupMessages({
  library(lmerTest)
  library(tidyr)
  library(spaMM)
  library(phylolm)
  library(ape)
  library(geiger)
  library(ggplot2)
})

source("correlations/helper.R")

model_df = read.csv(file = "data/cantometrics_ethnographicatlas.csv")

# remove all language families with <2 languages
tt = table(model_df$FamilyLevGlottocode)
tt_idx = tt >= 2
big_languagefamilies = model_df[model_df$FamilyLevGlottocode %in% names(tt)[tt_idx],]

data.7 = big_languagefamilies %>% 
  drop_na(line_7, std_EA033)

#### Models ####


### More complex models
## Linguistic model
tree = read.tree('data/super_tree.nwk')
data.7LF = data.7[!duplicated(data.7$GlottoID),]
rownames(data.7LF) = data.7LF$GlottoID
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

#### Bivariate
fit.4.0 = lm(line_7 ~ std_EA033, data = pruned_data)

bivariate_line = c(
  "line_7 ~ std_EA033",
  round(coef(fit.4.0),2), 
  summary(fit.4.0)$coefficients[,4],
  round(AIC(fit.4.0), 2))



spatial_line = c(
  "line_7 ~ std_EA033",
  round(fixef(spatial_model),2), 
  round(pt(spatial_summary$beta_table[,3], nrow(pruned_data) - 5, lower.tail = FALSE)),
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

output = data.frame(rbind(bivariate_line, spatial_line, phylo_line))
output$n = nrow(pruned_data)
write.csv(output, file = "correlations/results/complex_line7.csv")


# plot of effect
plot_1 = ggplot(pruned_data, aes(y = line_7, 
                        x = std_EA033)) + 
  geom_jitter(alpha = 0.3, width = 0.02, height = 0.02) + 
  geom_abline(aes(intercept=0.19,slope=0.14)) + 
  theme(legend.position = "none") + 
  xlab("EA033: Maximum standardized") + 
  ylab("CV7: Maximum standardized") + 
  ggtitle("Musical organization of the orchestra (CV7) vs Jurisdictional hierarchy (EA033)")

ggsave(filename = 'figs/line7_modelplot.png', plot = plot_1)

