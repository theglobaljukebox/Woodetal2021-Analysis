## This file contains the modelling for the PCA variables

suppressMessages(library(dplyr))
suppressMessages(library(lmerTest))
library(ggplot2)
source('phylogenetic_analysis/helper.R')

model_df = read.csv(file = "data/cantometrics_ethnographicatlas.csv")

# remove all language families with <2 languages
tt = table(model_df$Language_family)
tt_idx = tt >= 2
model_df = model_df[model_df$Language_family %in% names(tt)[tt_idx],]

# n_distinct(model_df$Language_family)
# n_distinct(model_df$Division)

model_pca = model_df %>% 
  dplyr::select(musical_pc1, social_pc1, Language_family, Division) %>% 
  na.omit()
  
fit.1 = lm(musical_pc1 ~ 1, data = model_pca)

fit.2 = lm(musical_pc1 ~ social_pc1, data = model_pca)

fit.3 = lmer(musical_pc1 ~ social_pc1 + (1|Language_family), data = model_pca)

fit.4 = lmer(musical_pc1 ~ social_pc1 + (1|Division), data = model_pca)

fit.5 = lmer(musical_pc1 ~ social_pc1 + (1|Division) + 
               (social_pc1|Division), data = model_pca)


# AIC(fit.1, fit.2, fit.3, fit.4, fit.5)

summary(fit.4)

## output
out_line = model_output(list(fit.2, fit.3, fit.4),
                        "social_pc1",
                        "Musical PC1 ~ Social PC1")


write.csv(out_line, "phylogenetic_analysis/pca.csv")

# Random slopes plot
new_data = model_pca %>% 
  filter(Division %in% c("East Africa", "Melanesia", "Russia", "Japan",
                         "United States"))

new_data$fit = predict(fit.5, new_data)

plot_1 = ggplot(model_pca,  aes(y = musical_pc1, x = social_pc1)) + 
  geom_jitter(alpha = 0.3, width = 0.02, height = 0.02) + 
  geom_line(data = new_data, aes(y = fit, x = social_pc1, col = Division)) + 
  theme(legend.position = "bottom") + 
  xlab("Social complexity") + 
  ylab("Musical complexity") + 
  ggtitle("Musical PC1 vs Social PC1")

ggsave("figs/pca_randomslopesmodelplot.png", plot_1)