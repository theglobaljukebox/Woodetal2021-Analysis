# Line 23 models

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

# remove all language families with <5 languages
tt = table(model_df$FamilyLevGlottocode)
tt_idx = tt >= 2
big_languagefamilies = model_df[model_df$FamilyLevGlottocode %in% names(tt)[tt_idx],]

# N language families vs N Divisions
# n_distinct(big_languagefamilies$FamilyLevGlottocode)
# n_distinct(big_languagefamilies$Division)

data.23 = big_languagefamilies %>% 
  drop_na(line_23, std_class, std_caste, std_slavery)

#### Continuous + RE ####
### More complex models
## Linguistic model
tree = read.tree('data/super_tree.nwk')
data.23LF = data.23[!duplicated(data.23$GlottoID),]
rownames(data.23LF) = data.23LF$GlottoID
pruned = treedata(phy = tree, data = data.23LF)
pruned_data = data.frame(pruned$data)
pruned_data$line_23 = as.numeric(pruned_data$line_23)
pruned_data$std_class = as.numeric(pruned_data$std_class)
pruned_data$std_caste = as.numeric(pruned_data$std_caste)
pruned_data$std_slavery = as.numeric(pruned_data$std_slavery)
pruned_data$Society_latitude = as.numeric(pruned_data$Society_latitude)
pruned_data$Society_longitude = as.numeric(pruned_data$Society_longitude)
pruned_tree = pruned$phy

# Standardize branch lengths
pruned_tree$edge.length = pruned_tree$edge.length / max(pruned_tree$edge.length)

phylo_model = phylolm(formula = line_23 ~ std_class + std_caste + std_slavery, 
                      data = pruned_data, 
                      phy = pruned_tree, 
                      model = "lambda")
phylo_summary = summary(phylo_model)

## Spatial model
spatial_model = fitme(
  line_23 ~ std_class + std_caste + std_slavery + Matern(1 | Society_longitude + Society_latitude),
  data = pruned_data,
  fixed = list(nu = 0.5), 
  method="REML")

spatial_summary = summary(spatial_model)

sp_aic = AIC(spatial_model)

# Bivariate model
fit.23.1.3 = lm(line_23 ~ std_class + std_caste + std_slavery, data = pruned_data)

bivariate_line = c(
  "line_23 ~ std_class + std_caste + std_slavery",
  round(coef(fit.23.1.3),2), 
  summary(fit.23.1.3)$coefficients[,4],
  round(AIC(fit.23.1.3), 2))

# DF for p values are difficult w Random effects.
# Here we calculate df as n data points - n fixed effects - 4
spatial_line = c(
  "line_23 ~ std_class + std_caste + std_slavery",
  round(fixef(spatial_model),2), 
  round(pt(spatial_summary$beta_table[,3], nrow(data.23) - 7, lower.tail = FALSE)),
  sp_aic[1])
names(spatial_line) = c("model", "Intercept", "class", "caste", "slavery", "intercept-p", "class-p", "caste-p", "slavery-p", "AIC")

phylo_line = c(
  "line_23 ~ std_class + std_caste + std_slavery",
  round(phylo_summary$coefficients[,1],2), 
  phylo_summary$coefficients[,4],
  AIC(phylo_model)
)
names(phylo_line) = c("model", "Intercept", "class", "caste", "slavery", "intercept-p", "class-p", "caste-p", "slavery-p", "AIC")

output = data.frame(rbind(bivariate_line, spatial_line, phylo_line))
output$n = nrow(pruned_data)

write.csv(output, 
          file = "correlations/results/complex_line23.csv")

# plot of effect

plot_1 = ggplot(pruned_data, aes(y = line_23, 
                                 x = std_class)) + 
  geom_jitter(alpha = 0.3, width = 0.02, height = 0.02) + 
  ## high-high
  geom_abline(aes(intercept=0.1 + 0.25 + 0.07,slope=0.18, col = "High-High")) +
  ## High-low
  geom_abline(aes(intercept=0.1 + 0.25,slope=0.18, col = "High-Low")) +
  ## Low-High
  geom_abline(aes(intercept=0.1 + 0.07,slope=0.18, col = "Low-High")) +
  ## low-low
  geom_abline(aes(intercept=0.1,slope=0.18, col = "Low-Low")) +
  theme(legend.position = "bottom") + 
  scale_color_manual(values = c("#E41A1C", "#377EB8", 
                                "#4DAF4A", "#984EA3"),
                     name = "Caste - Slavery") + 
  xlab("EA066: Maximum standardized") + 
  ylab("CV23: Maximum standardized") + 
  ggtitle("Embellishment (CV23) vs Class (EA066)")

plot_2 = ggplot(pruned_data, aes(y = line_23, 
                                 x = std_caste)) + 
  geom_jitter(alpha = 0.3, width = 0.02, height = 0.02) + 
  ## high-high
  geom_abline(aes(intercept=0.1 + 0.18 + 0.07,slope=0.25, col = "High-High")) +
  ## High-low
  geom_abline(aes(intercept=0.1 + 0.18,slope=0.25, col = "High-Low")) +
  ## Low-High
  geom_abline(aes(intercept=0.1 + 0.07,slope=0.25, col = "Low-High")) +
  ## low-low
  geom_abline(aes(intercept=0.1,slope=0.25, col = "Low-Low")) +
  theme(legend.position = "bottom") + 
  scale_color_manual(values = c("#E41A1C", "#377EB8", 
                                "#4DAF4A", "#984EA3"),
                     name = "Class - Slavery") + 
  xlab("EA068: Maximum standardized") + 
  ylab("CV23: Maximum standardized") + 
  ggtitle("Embellishment (CV23) vs Caste (EA068)")

plot_3 = ggplot(pruned_data, aes(y = line_23, 
                                 x = std_slavery)) + 
  geom_jitter(alpha = 0.3, width = 0.02, height = 0.02) + 
  ## high-high
  geom_abline(aes(intercept=0.1 + 0.18 + 0.25,slope=0.07, col = "High-High")) +
  ## High-low
  geom_abline(aes(intercept=0.1 + 0.18,slope=0.07, col = "High-Low")) +
  ## Low-High
  geom_abline(aes(intercept=0.1 + 0.25,slope=0.07, col = "Low-High")) +
  ## low-low
  geom_abline(aes(intercept=0.1,slope=0.07, col = "Low-Low")) +
  theme(legend.position = "bottom") + 
  scale_color_manual(values = c("#E41A1C", "#377EB8", 
                                "#4DAF4A", "#984EA3"),
                     name = "Class - Slavery") + 
  xlab("EA070: Maximum standardized") + 
  ylab("CV23: Maximum standardized") + 
  ggtitle("Embellishment (CV23) vs Slavery (EA070)")


ggsave('figs/line23class_modelplot.png', plot_1)


ggsave('figs/line23caste_modelplot.png', plot_2)


ggsave('figs/line23slavery_modelplot.png', plot_3)

pt(spatial_summary$beta_table[,3], nrow(data.23) - 7, lower.tail = FALSE)
