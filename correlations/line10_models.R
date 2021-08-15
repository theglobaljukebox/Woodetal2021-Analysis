# Line 10 models

suppressMessages(library(tidyr))
suppressMessages(library(lmerTest))
suppressMessages(library(dplyr))
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

data.10 = big_languagefamilies %>% 
  drop_na(line_10, std_subsistence)

#### Continuous + RE ####

fit.10.1.3 = lm(line_10 ~ std_subsistence, data = data.10)
fit.10.2.3 = lmer(line_10 ~ std_subsistence + (1|Language_family), data = data.10)
fit.10.3.3 = lmer(line_10 ~ std_subsistence + (1|Division), data = data.10)
fit.10.3.4 = lmer(line_10 ~ std_subsistence + (1|Division) + 
                    (std_subsistence|Division), data = data.10)


out_line = model_output(list(fit.10.1.3, fit.10.2.3, fit.10.3.3), "std_subsistence", "Line 10 ~ Subsistence")

write.csv(out_line, "correlations/results/line10.csv")

# plot of effect
data.10$fit <- predict(fit.10.3.3)   
# Add model fits to dataframe

# intercept_data = data.frame(std_subsistence = 0,
#                           Division = unique(data.10$Division))
# 
# intercept_data$intercept = predict(fit.10.3.3, intercept_data)

new_data = data.frame(std_subsistence = 
                        seq(from = 0, to = 1, 
                            length.out = 100),
                      Division = "East Africa")

new_data$fit = predict(fit.10.1.3, new_data)

plot_1 = ggplot(data.10, aes(y = line_10, 
                              x = std_subsistence)) + 
  # geom_point(aes(y=intercept, x = 0, group = Division), 
  #            size = 3, alpha = 0.5, pch = "_", 
  #            data = intercept_data, 
  #            col = "#CF4520") + 
  ylim(c(0,1)) +
  geom_line(aes(y = fit), data = new_data) +
  geom_jitter(alpha = 0.3, width = 0.02, height = 0.02) + 
  theme(legend.position = "none") + 
  xlab("Subsistence: Maximum standardized") + 
  ylab("CV10: Maximum standardized") + 
  ggtitle("Text Repetition (CV10) vs Subsistence")

ggsave(filename = 'figs/line10_modelplot.png', plot = plot_1)

