# Line 37 models
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

# remove all language families with <2 languages
tt = table(model_df$Language_family)
tt_idx = tt >= 2
big_languagefamilies = model_df[model_df$Language_family %in% names(tt)[tt_idx],]

# N language families vs N Divisions
# n_distinct(big_languagefamilies$Language_family)
# n_distinct(big_languagefamilies$Division)

data.37 = big_languagefamilies %>% 
  drop_na(line_37, std_EA033)

#### Continuous + RE ####

fit.37.1.3 = lm(line_37 ~ std_EA033, data = data.37)
fit.37.2.3 = lmer(line_37 ~ std_EA033 + (1|Language_family), data = data.37)
fit.37.3.3 = lmer(line_37 ~ std_EA033 + (1|Division), data = data.37)

out_line = model_output(list(fit.37.1.3, fit.37.2.3, fit.37.3.3),
                        "std_EA033",
                        "Line 37 ~ EA033")


write.csv(out_line, "correlations/results/line37.csv")

### More complex models
## Linguistic model
tree = read.tree('data/super_tree.nwk')
data.37LF = data.37[!duplicated(data.37$Glottocode),]
rownames(data.37LF) = data.37LF$Glottocode
pruned = treedata(phy = tree, data = data.37LF)
pruned_data = data.frame(pruned$data)
pruned_data$line_37 = as.numeric(pruned_data$line_37)
pruned_data$std_EA033 = as.numeric(pruned_data$std_EA033)
pruned_data$Society_latitude = as.numeric(pruned_data$Society_latitude)
pruned_data$Society_longitude = as.numeric(pruned_data$Society_longitude)
pruned_tree = pruned$phy

# Standardize branch lengths
pruned_tree$edge.length = pruned_tree$edge.length / max(pruned_tree$edge.length)

phylo_model = phylolm(formula = line_37 ~ std_EA033, 
                      data = pruned_data, 
                      phy = pruned_tree, 
                      model = "lambda")
phylo_summary = summary(phylo_model)

## Spatial model
spatial_model = fitme(
  line_37 ~ std_EA033 + Matern(1 | Society_longitude + Society_latitude),
  data = pruned_data,
  fixed = list(nu = 0.5), 
  method="REML")

spatial_summary = summary(spatial_model)

sp_aic = AIC(spatial_model)

spatial_line = c(
  "line_37 ~ std_EA033",
  round(fixef(spatial_model),2), 
  round(pt(spatial_summary$beta_table[,3], nrow(data.37) - 5, lower.tail = FALSE)),
  sp_aic[1])
names(spatial_line) = c("model", "Intercept", "Beta", 
                        "intercept-p", "beta-p",
                        "AIC")

phylo_line = c(
  "line_37 ~ std_EA033",
  round(phylo_summary$coefficients[,1],2), 
  phylo_summary$coefficients[,4],
  AIC(phylo_model)
)
names(phylo_line) = c("model", "Intercept", "Beta", 
                      "intercept-p", "beta-p",
                      "AIC")

write.csv(rbind(spatial_line, phylo_line), file = "correlations/results/complex_line37.csv")


# plot of effect
data.37$fit <- predict(fit.37.2.3)   
# Add model fits to dataframe

intercept_data = data.frame(std_EA033 = 0,
                            Language_family = 
                              unique(data.37$Language_family))

intercept_data$intercept = predict(fit.37.2.3, intercept_data)

new_data = data.frame(std_EA033 = 
                        seq(from = 0, to = 1, 
                            length.out = 100),
                      Language_family = "Indo-European")

new_data$fit = predict(fit.37.2.3, new_data)

plot_1 = ggplot(data.37, aes(y = line_37, 
                             x = std_EA033)) + 
  geom_point(aes(y=intercept, x = 0, 
                 group = Language_family), 
             size = 3, alpha = 0.5, pch = "_", 
             data = intercept_data, 
             col = "#CF4520") + 
  ylim(c(0,1)) +
  geom_line(aes(y = fit), data = new_data) +
  geom_jitter(alpha = 0.3, width = 0.02, height = 0.02) + 
  theme(legend.position = "none") + 
  xlab("EA033: Maximum standardized") + 
  ylab("CV37: Maximum standardized") + 
  ggtitle("Enunciation (CV37) vs Jurisdictional hierarchy (EA033)",
          "Language family:Indo-European regression line")

ggsave(filename = 'figs/line37_modelplot.png', plot = plot_1)
