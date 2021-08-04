# Line 21 models

suppressMessages(library(tidyr))
suppressMessages(library(lmerTest))
suppressMessages(library(dplyr))
library(ggplot2)
source("phylogenetic_analysis/helper.R")

model_df = read.csv(file = "data/cantometrics_ethnographicatlas.csv")

# remove all language families with <5 languages
tt = table(model_df$Language_family)
tt_idx = tt >= 2
big_languagefamilies = model_df[model_df$Language_family %in% names(tt)[tt_idx],]

# N language families vs N Divisions
# n_distinct(big_languagefamilies$Language_family)
# n_distinct(big_languagefamilies$Division)

data.21 = big_languagefamilies %>% 
  drop_na(line_21, std_EA031, Division)

#### Continuous + RE ####

fit.21.1.3 = lm(line_21 ~ std_EA031, data = data.21)
fit.21.2.3 = lmer(line_21 ~ std_EA031 + (1|Language_family), data = data.21)
fit.21.3.3 = lmer(line_21 ~ std_EA031 + (1|Division), data = data.21)

out_line = model_output(list(fit.21.1.3, fit.21.2.3, fit.21.3.3), "std_EA031", "Line 21 ~ EA031")

write.csv(out_line, "phylogenetic_analysis/line21.csv")


# plot of effect
data.21$fit <- predict(fit.21.3.3)   
# Add model fits to dataframe

intercept_data = data.frame(std_EA031 = 0,
                            Division = unique(data.21$Division))

intercept_data$intercept = predict(fit.21.3.3, intercept_data)

new_data = data.frame(std_EA031 = 
                        seq(from = 0, to = 1, 
                            length.out = 100),
                      Division = "East Africa")

new_data$fit = predict(fit.21.3.3, new_data)

plot_1 = ggplot(data.21, aes(y = line_21, 
                            x = std_EA031)) + 
  geom_point(aes(y=intercept, x = 0, group = Division), 
             size=3, alpha = 0.5, pch = "_", 
             data = intercept_data, col = "#CF4520") + 
  ylim(c(0,1)) +
  geom_line(aes(y = fit), data = new_data) +
  geom_jitter(alpha = 0.3, width = 0.02, height = 0.02) + 
  theme(legend.position = "none") + 
  xlab("EA031: Maximum standardized") + 
  ylab("Line 21: Maximum standardized") + 
  ggtitle("Melodic interval size (Line 21) vs Community Size (EA031)",
          "Division:East Africa regression line")

ggsave(filename = 'figs/line21_modelplot.png', plot = plot_1)

