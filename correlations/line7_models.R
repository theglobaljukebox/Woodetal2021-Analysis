# Line 7 models
suppressMessages(library(lmerTest))
suppressMessages(library(tidyr))
library(ggplot2)

source("correlations/helper.R")

model_df = read.csv(file = "data/cantometrics_ethnographicatlas.csv")

# remove all language families with <5 languages
tt = table(model_df$Language_family)
tt_idx = tt >= 2
big_languagefamilies = model_df[model_df$Language_family %in% names(tt)[tt_idx],]

data.3 = big_languagefamilies %>% 
  drop_na(line_7, std_EA033)

#### Continuous + RE ####

fit.4.0 = lm(line_7 ~ std_EA033, data = data.3)
fit.4.1 = lmer(line_7 ~ std_EA033 + (1|Language_family), data = data.3)
fit.4.2 = lmer(line_7 ~ std_EA033 + (1|Division), data = data.3)

out_line = model_output(list(fit.4.0, fit.4.1, fit.4.2), "std_EA033", "CV7 ~ EA033")

write.csv(out_line, "correlations/results/line7.csv")

# plot of effect
data.3$fit <- predict(fit.4.2)   
# Add model fits to dataframe

intercept_data = data.frame(std_EA033 = 0,
                            Division = unique(data.3$Division))

intercept_data$intercept = predict(fit.4.2, intercept_data)

new_data = data.frame(std_EA033 = 
                        seq(from = 0, to = 1, 
                            length.out = 100),
                      Division = "East Africa")

new_data$fit = predict(fit.4.2, new_data)

plot_1 = ggplot(data.3, aes(y = line_7, 
                             x = std_EA033)) + 
  geom_point(aes(y=intercept, x = 0, group = Division), 
             size=3, alpha = 0.5, pch = "_", 
             data = intercept_data,
             col = "#CF4520") + 
  ylim(c(0,1)) +
  geom_line(aes(y = fit), data = new_data) +
  geom_jitter(alpha = 0.3, width = 0.02, height = 0.02) + 
  theme(legend.position = "none") + 
  xlab("EA033: Maximum standardized") + 
  ylab("CV7: Maximum standardized") + 
  ggtitle("Musical organization of the orchestra (CV7) vs Jurisdictional hierarchy (EA033)",
          "Division:East Africa regression line")

ggsave(filename = 'figs/line7_modelplot.png', plot = plot_1)
