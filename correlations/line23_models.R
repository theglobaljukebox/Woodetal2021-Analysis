# Line 23 models

suppressMessages(library(lmerTest))
suppressMessages(library(tidyr))
suppressMessages(library(spaMM))
suppressMessages(library(dplyr))
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

data.23 = big_languagefamilies %>% 
  drop_na(line_23, std_class, std_caste, std_slavery)

#### Continuous + RE ####
fit.23.1.3 = lm(line_23 ~ std_class + std_caste + std_slavery, data = data.23)
fit.23.2.3 = lmer(line_23 ~ std_class + std_caste + std_slavery + (1|Language_family), data = data.23)
fit.23.3.3 = lmer(line_23 ~ std_class + std_caste + std_slavery + (1|Division), data = data.23)

out_line = model_output(list(fit.23.1.3, fit.23.2.3, fit.23.3.3),
                        c("std_class", "std_caste", "std_slavery"),
                        "CV23 ~ Class + Caste + Slavery")

write.csv(out_line, "correlations/results/line23.csv")

### More complex models
## Linguistic model
tree = read.tree('data/super_tree.nwk')
data.23LF = data.23[!duplicated(data.23$Glottocode),]
rownames(data.23LF) = data.23LF$Glottocode
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

write.csv(rbind(spatial_line, phylo_line), file = "correlations/results/complex_line23.csv")

# plot of effect
data.23$fit <- predict(fit.23.3.3) # Add model fits to dataframe

intercept_data = data.frame(std_class = 0,
                            std_caste = 0,
                            std_slavery = 0,
                            Division = unique(data.23$Division))

intercept_data$intercept = predict(fit.23.3.3, intercept_data)

# Line data
# vector of high medium and low settings for all variables
# 
# Class plot
new_data = data.frame(std_class = rep(seq(0, 1, length.out = 25), 4),
                      std_caste = rep(c(0, 1, 0, 1), 25),
                      std_slavery = rep(c(0, 0, 1, 1), 25),
                      Division = rep("East Africa", 100))

rankings = data.frame(std_caste = c(0, 1, 0, 1),
                      std_slavery = c(0, 0, 1, 1),
                      label = c("Low-Low", "High-Low", "Low-High", 
                      "High-High"))

new_data = left_join(new_data, rankings, 
                     by = c("std_caste", "std_slavery"))

new_data$fit = predict(fit.23.3.3, new_data)

plot_1 = ggplot(data.23,  aes(y = line_23, x = std_class)) + 
  geom_point(aes(y=fit, x = -0.05, group = Language_family), 
             size=3, alpha = 0.5, pch = "_", col = "#CF4520") +
  geom_jitter(alpha = 0.3, width = 0.02, height = 0.02) + 
  geom_line(aes(y = fit, col = label), data = new_data) + 
  scale_color_discrete(name = "Caste - Slavery") +
  theme(legend.position = "bottom") + 
  xlab("EA066: Maximum standardized") + 
  ylab("CV23: Maximum standardized") + 
  ggtitle("Embellishment (CV23) vs Class (EA066)",
          "Division:East Africa regression line")

ggsave('figs/line23class_modelplot.png', plot_1)

# Caste plot
new_data = data.frame(std_caste = rep(seq(0, 1, length.out = 25), 4),
                      std_class = rep(c(0, 1, 0, 1), 25),
                      std_slavery = rep(c(0, 0, 1, 1), 25),
                      Division = rep("East Africa", 100))

rankings = data.frame(std_class= c(0, 1, 0, 1),
                      std_slavery = c(0, 0, 1, 1),
                      label = c("Low-Low", "High-Low", "Low-High", 
                                "High-High"))

new_data = left_join(new_data, rankings, 
                     by = c("std_class", "std_slavery"))

new_data$fit = predict(fit.23.3.3, new_data)

plot_2 = ggplot(data.23,  aes(y = line_23, x = std_caste)) + 
  geom_point(aes(y=fit, x = -0.05, group = Language_family), 
             size=3, alpha = 0.5, pch = "_", col = "#CF4520") +
  geom_jitter(alpha = 0.3, width = 0.02, height = 0.02) + 
  geom_line(aes(y = fit, col = label), data = new_data) + 
  scale_color_discrete(name = "Class - Slavery") + 
  theme(legend.position = "bottom") + 
  xlab("EA068: Maximum standardized") + 
  ylab("CV23: Maximum standardized") + 
  ggtitle("Embellishment (CV23) vs Caste (EA068)")

ggsave('figs/line23caste_modelplot.png', plot_2)

## Slavery
new_data = data.frame(std_slavery = rep(seq(0, 1, length.out = 25), 4),
                      std_class = rep(c(0, 1, 0, 1), 25),
                      std_caste = rep(c(0, 0, 1, 1), 25),
                      Division = rep("East Africa", 100))

rankings = data.frame(std_class= c(0, 1, 0, 1),
                      std_caste = c(0, 0, 1, 1),
                      label = c("Low-Low", "High-Low", "Low-High", 
                                "High-High"))

new_data = left_join(new_data, rankings, 
                     by = c("std_class", "std_caste"))

new_data$fit = predict(fit.23.3.3, new_data)

plot_3 = ggplot(data.23,  aes(y = line_23, x = std_slavery)) + 
  geom_point(aes(y=fit, x = -0.05, group = Language_family), 
             size=3, alpha = 0.5, pch = "_", col = "#CF4520") +
  geom_jitter(alpha = 0.3, width = 0.02, height = 0.02) + 
  geom_line(aes(y = fit, col = label), data = new_data) + 
  scale_color_discrete(name = "Class - Caste") + 
  theme(legend.position = "bottom") + 
  xlab("EA070: Maximum standardized") + 
  ylab("CV23: Maximum standardized") + 
  ggtitle("Embellishment (CV23) vs Slavery (EA070)")


ggsave('figs/line23slavery_modelplot.png', plot_3)
