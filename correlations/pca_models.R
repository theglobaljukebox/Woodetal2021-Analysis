## This file contains the modelling for the PCA variables

suppressMessages(library(tidyr))
suppressMessages(library(lmerTest))
suppressMessages(library(spaMM))
suppressMessages(library(dplyr))
suppressMessages(library(ape))
suppressMessages(library(geiger))
suppressMessages(library(phylolm))
library(ggplot2)
library(grid)
source("correlations/helper.R")


model_df = read.csv(file = "data/cantometrics_ethnographicatlas.csv")

# remove all language families with <2 languages
tt = table(model_df$Language_family)
tt_idx = tt >= 2
model_df = model_df[model_df$Language_family %in% names(tt)[tt_idx],]

# n_distinct(model_df$Language_family)
# n_distinct(model_df$Division)

model_pca = model_df %>% 
  dplyr::select(musical_pc1, social_pc1, Language_family, Division, EA_code, EA031, Glottocode, Society_latitude, Society_longitude) %>% 
  na.omit()
  

### More complex models
## Linguistic model
tree = read.tree('data/super_tree.nwk')
model_pcaLF = model_pca[!duplicated(model_pca$Glottocode),]
rownames(model_pcaLF) = model_pcaLF$Glottocode
pruned = treedata(phy = tree, data = model_pcaLF)
pruned_data = data.frame(pruned$data)
pruned_data$musical_pc1 = as.numeric(pruned_data$musical_pc1)
pruned_data$social_pc1 = as.numeric(pruned_data$social_pc1)
pruned_data$Society_latitude = as.numeric(pruned_data$Society_latitude)
pruned_data$Society_longitude = as.numeric(pruned_data$Society_longitude)
pruned_tree = pruned$phy

# Standardize branch lengths
pruned_tree$edge.length = pruned_tree$edge.length / max(pruned_tree$edge.length)

phylo_model = phylolm(formula = musical_pc1 ~ social_pc1, 
                      data = pruned_data, 
                      phy = pruned_tree, 
                      model = "lambda")
phylo_summary = summary(phylo_model)

## Spatial model
spatial_model = fitme(
  musical_pc1 ~ social_pc1 + Matern(1 | Society_longitude + Society_latitude),
  data = pruned_data,
  fixed = list(nu = 0.5), 
  method="REML")

spatial_summary = summary(spatial_model)

sp_aic = AIC(spatial_model)

# bivariate model
fit.2 = lm(musical_pc1 ~ social_pc1, data = pruned_data)

bivariate_line = c(
  "musical_pc1 ~ social_pc1",
  round(coef(fit.2),2), 
  summary(fit.2)$coefficients[,4],
  round(AIC(fit.2), 2))

spatial_line = c(
  "musical_pc1 ~ social_pc1",
  round(fixef(spatial_model),2), 
  round(pt(spatial_summary$beta_table[,3], nrow(model_pca) - 5, lower.tail = FALSE)),
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
write.csv(output, file = "correlations/results/complex_pca.csv")

# Random slopes plot
new_data = model_pca %>% 
  filter(Division %in% c("East Africa", "Melanesia", "Southern Europe", "Island S E Asia",
                         "United States"))

new_data$fit = predict(fit.2, new_data)

plot_1 = ggplot(model_pca,  aes(y = musical_pc1, x = social_pc1))  + 
  xlab("Social complexity PC1 (~58% of variance in Ethnographic Atlas variables)") + 
  ylab("Song style PC1 (~45% of variance in Cantometrics variables)") +
  annotation_custom(
    grob = textGrob(label = "Low social complexity", 
                    gp = gpar(cex = 0.8)), 
    xmin = -1, xmax = -1, 
    ymin = -2.8, ymax = -2.8) +
  annotation_custom(
    grob = textGrob(label = "High social complexity", 
                    gp = gpar(cex = 0.8)), 
    xmin = 1.9, xmax = 1.9, 
    ymin = -2.8, ymax = -2.8) +
  annotation_custom(
    grob = textGrob(label = "High musical complexity", 
                    gp = gpar(fontsize = 14), rot = 90), 
    xmin = -1.7, xmax = -1.7, 
    ymin = 1.8, ymax = 1.8) +
  annotation_custom(
    grob = textGrob(label = "Low musical complexity", 
                    gp = gpar(fontsize = 14), rot = 90), 
    xmin = -1.7, xmax = -1.7, 
    ymin = -1.5, ymax = -1.5) + 
  geom_point(alpha = 0.3) +
  theme(legend.position = "bottom",
        plot.margin = unit(c(1,1,2,2), "lines"),
        text = element_text(size=18))

gt <- ggplot_gtable(ggplot_build(plot_1))
gt$layout$clip[gt$layout$name == "panel"] <- "off"
grid.draw(gt)

ggsave("figs/pca_plot.png", gt, width = 10, height = 8)
