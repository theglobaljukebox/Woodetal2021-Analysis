## This file contains the modelling for the PCA variables

suppressMessages(library(dplyr))
suppressMessages(library(lmerTest))
library(ggplot2)
library(grid)
source('correlations/helper.R')

model_df = read.csv(file = "data/cantometrics_ethnographicatlas.csv")

# remove all language families with <2 languages
tt = table(model_df$Language_family)
tt_idx = tt >= 2
model_df = model_df[model_df$Language_family %in% names(tt)[tt_idx],]

# n_distinct(model_df$Language_family)
# n_distinct(model_df$Division)

model_pca = model_df %>% 
  dplyr::select(musical_pc1, social_pc1, Language_family, Division, EA_code, EA031) %>% 
  na.omit()
  
fit.1 = lm(musical_pc1 ~ 1, data = model_pca)

fit.2 = lm(musical_pc1 ~ social_pc1, data = model_pca)

fit.3 = lmer(musical_pc1 ~ social_pc1 + (1|Language_family), data = model_pca)

fit.4 = lmer(musical_pc1 ~ social_pc1 + (1|Division), data = model_pca)

fit.5 = lmer(musical_pc1 ~ social_pc1 + (1|Division) + 
               (social_pc1|Division), data = model_pca)


# AIC(fit.1, fit.2, fit.3, fit.4, fit.5)

# summary(fit.4)

## output
out_line = model_output(list(fit.2, fit.3, fit.4),
                        "social_pc1",
                        "Musical PC1 ~ Social PC1")


write.csv(out_line, "correlations/results/pca.csv")

# Random slopes plot
new_data = model_pca %>% 
  filter(Division %in% c("East Africa", "Melanesia", "Southern Europe", "Island S E Asia",
                         "United States"))

new_data$fit = predict(fit.5, new_data)

plot_1 = ggplot(model_pca,  aes(y = musical_pc1, x = social_pc1)) + 
  theme(legend.position = "bottom",
        plot.margin = unit(c(1,1,2,2), "lines")) + 
  xlab("Social complexity PC1 (~58% of variance in Ethnographic Atlas variables)") + 
  ylab("Song style PC1 (~41% of variance in Cantometrics variables)") +
  annotation_custom(
    grob = textGrob(label = "Low social complexity", 
                    gp = gpar(cex = 0.8)), 
    xmin = -1, xmax = -1, 
    ymin = -2.9, ymax = -2.9) +
  annotation_custom(
    grob = textGrob(label = "High social complexity", 
                    gp = gpar(cex = 0.8)), 
    xmin = 1.9, xmax = 1.9, 
    ymin = -2.9, ymax = -2.9) +
  annotation_custom(
    grob = textGrob(label = "High musical complexity", 
                    gp = gpar(cex = 0.8), rot = 90), 
    xmin = -1.8, xmax = -1.8, 
    ymin = 2.0, ymax = 2.0) +
  annotation_custom(
    grob = textGrob(label = "Low musical complexity", 
                    gp = gpar(cex = 0.8), rot = 90), 
    xmin = -1.8, xmax = -1.8, 
    ymin = -1.5, ymax = -1.5) + 
  geom_point(alpha = 0.3)

gt <- ggplot_gtable(ggplot_build(plot_1))
gt$layout$clip[gt$layout$name == "panel"] <- "off"
grid.draw(gt)

ggsave("figs/pca_randomslopesmodelplot_nolines.png", gt)