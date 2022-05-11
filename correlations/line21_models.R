# Line 21 models
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

data.21 = big_languagefamilies %>% 
  drop_na(line_21, std_EA031, Division)


#### Continuous + RE ####

### More complex models
## Linguistic model
tree = read.tree('data/super_tree.nwk')
data.21LF = data.21[!duplicated(data.21$GlottoID),]
rownames(data.21LF) = data.21LF$GlottoID
pruned = suppressWarnings(treedata(phy = tree, data = data.21LF))
pruned_data = data.frame(pruned$data)
pruned_data$line_21 = as.numeric(pruned_data$line_21)
pruned_data$std_EA031 = as.numeric(pruned_data$std_EA031)
pruned_data$Society_latitude = as.numeric(pruned_data$Society_latitude)
pruned_data$Society_longitude = as.numeric(pruned_data$Society_longitude)
pruned_tree = pruned$phy

x = assertthat::assert_that(nrow(pruned_data) == 221)

# Standardize branch lengths
pruned_tree$edge.length = pruned_tree$edge.length / max(pruned_tree$edge.length)

phylo_model = phylolm(formula = line_21 ~ std_EA031, 
                      data = pruned_data, 
                      phy = pruned_tree, 
                      model = "lambda")
phylo_summary = summary(phylo_model)

## Spatial model
spatial_model = fitme(
  line_21 ~ std_EA031 + Matern(1 | Society_longitude + Society_latitude),
  data = pruned_data,
  fixed = list(nu = 0.5), 
  method="REML")

spatial_summary = summary(spatial_model, verbose = FALSE)

sp_aic = AIC(spatial_model, verbose = FALSE)

# Bivariate data
fit.21.1.3 = lm(line_21 ~ std_EA031, data = pruned_data)

bivariate_line = c(
  "line_21 ~ std_EA031",
  round(coef(fit.21.1.3),2), 
  summary(fit.21.1.3)$coefficients[,4],
  round(AIC(fit.21.1.3), 2))

spatial_line = c(
  "line_21 ~ std_EA031",
  round(fixef(spatial_model),2), 
  round(pt(spatial_summary$beta_table[,3], nrow(data.21) - 5, lower.tail = FALSE)),
  sp_aic[1])
names(spatial_line) = c("model", "Intercept", "Beta", 
                        "intercept-p", "beta-p",
                        "AIC")

phylo_line = c(
  "line_21 ~ std_EA031",
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
          file = "correlations/results/complex_line21.csv")

# plot of effect
plot_1 = ggplot(pruned_data, aes(y = line_21, 
                        x = std_EA031)) + 
  geom_jitter(alpha = 0.3, width = 0.02, height = 0.02) + 
  geom_abline(aes(intercept=0.64,slope=-0.14)) + 
  theme(legend.position = "none") + 
  xlab("EA031: Maximum standardized") + 
  ylab("CV21: Maximum standardized") + 
  ggtitle("Melodic interval size (CV21) vs Community Size (EA031)")

ggsave(filename = 'figs/line21_modelplot.png', plot = plot_1)

