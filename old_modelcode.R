old model code

big_languagefamilies$line_23 = ordered(big_languagefamilies$line_23)

fit.23.0 <- vglm(line_23 ~ 1, 
                 family = propodds,
                 data = data.23)

fit.23.1 <- vglm(line_23 ~ std_class + std_caste + std_slavery, 
                 family = propodds,
                 data = data.23)

fit.23.1.1 = multinom(as.factor(line_23) ~ std_class + std_caste + std_slavery, 
                      data = data.23)

fit.23.1.2 = lm(as.numeric(line_23) ~ std_class + std_caste + std_slavery, 
                data = data.23)

# Ordinal assumption is about the same as other assumptions - so continue with ordinal
AIC(fit.23.1) # Ordinal
AIC(fit.23.1.1) # Categorical
AIC(fit.23.1.2) # Linear / Continuous

#### Language Control ####
fit.23.2 <- vglm(line_23 ~ std_class + std_caste + std_slavery + Language_family, 
                 family = propodds,
                 data = data.23)

fit.23.2.1 = multinom(as.factor(line_23) ~ std_class + std_caste + std_slavery  + Language_family, 
                      data = data.23)

fit.23.2.2 = lm(as.numeric(line_23) ~ std_class + std_caste + std_slavery  + Language_family, 
                data = data.23)

# Ordinal assumption is is the best fit
AIC(fit.23.2) # Ordinal
AIC(fit.23.2.1) # Categorical
AIC(fit.23.2.2) # Linear / Continuous

#### Diffusional Division ####
fit.23.3 <- vglm(line_23 ~ std_class + std_caste + std_slavery + Division, 
                 family = propodds,
                 data = data.23)

fit.23.3.1 = multinom(as.factor(line_23) ~ std_class + std_caste + std_slavery + Division, 
                      data = data.23)

fit.23.3.2 = lm(as.numeric(line_23) ~ std_class + std_caste + std_slavery + Division, 
                data = data.23)

# Ordinal assumption is is the best fit
AIC(fit.23.3) # Ordinal
AIC(fit.23.3.1) # Categorical
AIC(fit.23.3.2) # Linear / Continuous

# Transmission effects
# Horizontal transmission is the best model 
AIC(fit.23.1) # No transmission
AIC(fit.23.2) # Vertical transmission
AIC(fit.23.3) # Horizontal transmission

summary(fit.23.3)