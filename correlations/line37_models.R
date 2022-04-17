# Line 37 models
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

# N language families vs N Divisions
# n_distinct(big_languagefamilies$FamilyLevGlottocode)
# n_distinct(big_languagefamilies$Division)

data.37 = big_languagefamilies %>% 
  drop_na(line_37, std_EA033)

#### Continuous + RE ####

### More complex models
## Linguistic model
tree = read.tree('data/super_tree.nwk')
data.37LF = data.37[!duplicated(data.37$GlottoID),]
rownames(data.37LF) = data.37LF$GlottoID
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


# Bivariate 
fit.37.1.3 = lm(line_37 ~ std_EA033, data = pruned_data)

bivariate_line = c(
  "line_37 ~ std_EA033",
  round(coef(fit.37.1.3),2), 
  summary(fit.37.1.3)$coefficients[,4],
  round(AIC(fit.37.1.3), 2))

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

output = data.frame(rbind(bivariate_line, spatial_line, phylo_line))
output$n = nrow(pruned_data)

write.csv(output, 
          file = "correlations/results/complex_line37.csv")


# plot of effect

plot_1 = ggplot(pruned_data, aes(y = line_37, 
                                 x = std_EA033)) + 
  geom_jitter(alpha = 0.3, width = 0.02, height = 0.02) + 
  geom_abline(aes(intercept=0.37,slope=0.18)) + 
  theme(legend.position = "none") + 
  xlab("EA033: Maximum standardized") + 
  ylab("CV37: Maximum standardized") + 
  ggtitle("Enunciation (CV37) vs Jurisdictional hierarchy (EA033)")

ggsave(filename = 'figs/line37_modelplot.png', plot = plot_1)
