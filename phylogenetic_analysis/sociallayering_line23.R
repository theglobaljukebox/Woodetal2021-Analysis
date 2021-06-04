library(geiger)
library(ape)
library(dplyr)
library(RColorBrewer)
library(phylolm)
library(lmerTest)
library(sjPlot)
library(sjmisc)
library(ggplot2)
library(ggeffects)

#### Script ####

tree_url = "https://raw.githubusercontent.com/D-PLACE/dplace-data/master/phylogenies/gray_et_al2009/summary.trees"
taxa_url = "https://raw.githubusercontent.com/D-PLACE/dplace-data/master/phylogenies/gray_et_al2009/taxa.csv"
name = "Austronesia"

# ---- Data ---- #
ea_cantometrics = read.csv('data/socialfactors_cantomtericsmodalprofiles.csv')

tree = read.nexus(tree_url)
taxa = read.csv(taxa_url)

# We are not interested in OldJavanese
tree = drop.tip(tree, "OldJavanese")
### Standardize branch lengths
tree$edge.length = tree$edge.length / max(tree$edge.length)

#### Analysis #### 
### Global ###
relevel_line23 = data.frame(line_23 = sort(unique(ea_cantometrics$line_23)),
                            line_23ordinal = 1:(length(unique(ea_cantometrics$line_23))-1))

ea_cantometrics = left_join(ea_cantometrics, relevel_line23, "line_23")

t = table(ea_cantometrics$Language_family) 
big_languagefamily = names(t)[t > 5]

large_languagefamily = ea_cantometrics[ea_cantometrics$Language_family %in% big_languagefamily,]

ggplot(large_languagefamily, aes(x = SocialFactors_V33_code, y = line_23ordinal)) + 
  geom_jitter(position = position_jitter(width = 0.1, height = 0.1))

large_languagefamily = large_languagefamily %>% 
  select(line_23ordinal, SocialFactors_V33_code, Language_family, Division) %>% 
  na.omit

# fit_simple <- brm(line_23ordinal ~ SocialFactors_V33_code, data=large_languagefamily, 
#                   family=cumulative("logit"), 
#                   chain = 2, cores = 2)
fit_simple = lm(line_23ordinal ~ SocialFactors_V33_code, data = large_languagefamily)
fit_languagefam = lmer(line_23ordinal ~ SocialFactors_V33_code + (1|Language_family), data = large_languagefamily)
fit_region = lmer(line_23ordinal ~ SocialFactors_V33_code + (1|Division), data = large_languagefamily)
fit_both = lmer(line_23ordinal ~ SocialFactors_V33_code + (1|Division) + (1|Language_family), data = large_languagefamily)

plot_model(fit_languagefam, y.offset = .4, type = "re")
plot_model(fit_region, y.offset = .4, type = "re")
plot_model(fit_both, y.offset = .4, type = "re")

# Model comparison

anova(fit_region, fit_both, test="Chisq")
anova(fit_languagefam, fit_both, test="Chisq")

## Model predictions

  # install the package first if you haven't already, then load it

# Extract the prediction data frame
pred.both <- ggpredict(fit_both, terms = c("SocialFactors_V33_code"))  # this gives overall predictions for the model
pred.both = predict(fit_both)

plot(large_languagefamily$line_23ordinal[!is.na(large_languagefamily$line_23ordinal)], 
     pred.both)

# Plot the predictions 

(ggplot(pred.both) + 
    geom_line(aes(x = x, y = predicted)) +          # slope
    geom_ribbon(aes(x = x, ymin = predicted - std.error, ymax = predicted + std.error), 
                fill = "lightgrey", alpha = 0.5) +  # error band
    geom_jitter(data = large_languagefamily,                      # adding the raw data (scaled values)
               aes(x = SocialFactors_V33_code, y = line_23ordinal), ) + 
    labs(x = "Social Layering", y = "Embellishment") + 
    theme_minimal()
)

#### Regional ####
cantometrics = left_join(ea_cantometrics, taxa, by = c("Glottocode" = "glottocode"))
# we need to have only one row per society (despite there being multiple songs per society)
cantometrics = cantometrics[!duplicated(cantometrics$taxon),]
# we also only want to display austronesian languages
cantometrics = cantometrics[!is.na(cantometrics$taxon),]

# subset to valid data
cantometrics = cantometrics[!is.na(cantometrics$line_23) & !is.na(cantometrics$SocialFactors_V33_code),]

rownames(cantometrics) = cantometrics$taxon
plot_obj = treedata(phy = tree, data = cantometrics)

plot_obj$phy = ladderize(plot_obj$phy)
plot_obj$data = as.data.frame(plot_obj$data)

### Austronesian ###
model_df = plot_obj$data
model_df$line_23ordinal = as.numeric(model_df$line_23ordinal)
model_df$SocialFactors_V33_code = as.numeric(model_df$SocialFactors_V33_code)

trees = read.nexus("https://raw.githubusercontent.com/D-PLACE/dplace-data/master/phylogenies/gray_et_al2009/posterior.trees")
pruned_trees = lapply(trees, function(t) keep.tip(t, rownames(plot_obj$data)))

fit = lm(line_23ordinal ~ SocialFactors_V33_code, data = model_df)

fit_phylo = list()
for(i in seq_along(pruned_trees)){
  fit_phylo[[i]] = phylolm(line_23ordinal ~ SocialFactors_V33_code, data = model_df, phy = pruned_trees[[i]], model = "lambda")  
}

phylo_coefficients = lapply(fit_phylo, function(f) coef(summary(f)))

lambda = lapply(fit_phylo, function(s) s$sigma2) %>% unlist %>% mean

round(Reduce("+", phylo_coefficients) / length(phylo_coefficients), 2)

# png(paste0("trees/", name, "_socialfactorssociallayering_line23.png"), width = 200, height = 200, units='mm', res = 300)
pdf("trees/Austronesia_socialfactorssociallayering_line23.pdf")
par(xpd=NA) # stop clipping
# Line 23 and 4
plot(plot_obj$phy, font = 1, cex = 0.7, label.offset = 0.07,
     main = "Embellishment (Line 23) & Social Layering")
tiplabels(pch = 21, bg = plot_obj$data$line23_col, cex = 1.5)
tiplabels(pch = 21, bg = plot_obj$data$stratification_col, adj = 0.54, cex = 1.5)

left = 0.2
right = 1.3
n = 5
colfunc <- colorRampPalette( c("#5E9AC4", "#C52B2F"))
rect(head(seq(left, right, by = (right-left)/n), -1), 
     -2, 
     tail(seq(left, right, by = (right-left)/n), -1), 
     -0.5,
     # col = colfunc(5))
     col = c("#5E9AC4", "#88C1CA", "#F6C3C3", "#F1635B", "#C52B2F"))
text(x = c(left, right - 0.05), y = -2.05, c("Low Embellishment / \n No slavery, class, or caste distinctions",
  "Extreme Embellishment / \n Social layering score of five to six."), pos = 1)
  
dev.off()


