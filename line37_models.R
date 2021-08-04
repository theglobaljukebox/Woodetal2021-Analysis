# Line 37 models
suppressMessages(library(lmerTest))
suppressMessages(library(tidyr))
suppressMessages(library(dplyr))
library(ggplot2)
source("phylogenetic_analysis/helper.R")

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


write.csv(out_line, "phylogenetic_analysis/line37.csv")

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
  ylab("Line 37: Maximum standardized") + 
  ggtitle("Enunciation (Line 37) vs Jurisdictional hierarchy (EA033)",
          "Language family:Indo-European regression line")

ggsave(filename = 'figs/line37_modelplot.png', plot = plot_1)
