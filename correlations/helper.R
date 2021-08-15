get_stats = function(object, var, rr = 3){
  s = summary(object)
  if(isS4(s)){
    bp = s@coef3[var,c(1, 4)] # get beta and p-value  
  } else{
    bp = s$coefficients[var,c("Estimate","Pr(>|t|)")] # get beta and p-value
  }
  bp = matrix(bp, ncol = 2)
  # pvalue to stars
  pvalues = bp[,2]
  stars = ifelse(bp[,2] < 0.001, "\\*\\*\\*", 
                 ifelse(bp[,2] < 0.01, "\\*\\*",
                        ifelse(bp[,2] < 0.05, "\\*", "")))
  
  bp[,2] = stars
  
  # if(is.matrix(bp)){
    l = paste(signif(as.numeric(bp[,1]), rr), bp[,2], sep = "")  
    l = paste(l, collapse = "; ")
  # } else {
  #   l = paste(signif(bp[1], rr), " (", signif(bp[2], rr), ")", sep = "")
  # }
  l
}

model_output = function(objects, var, model_label){
  
  statistics = sapply(objects, get_stats, var)
  aic_values = sapply(objects, function(x) round(AIC(x), 2))
  n = nrow(objects[[1]]$model)
  
  out = c(model_label, N = n, statistics, aic_values)
  
  names(out) = c("Model", "N", "Bivariate", "Language family", "Division", "BV:AIC", "LF:AIC", "DV:AIC")
  
  out
}

musical_conversion = function(x, line_range){
  original_values = sapply(1:max(line_range), function(z) 2^z)
  coded_values = 1:max(line_range)
  
  matched_df = data.frame(coded_values = coded_values, 
                          original_values = original_values)
  
  x_df = data.frame(original_values = x)
  
  paired = dplyr::left_join(x_df, matched_df)
  
  (paired$coded_values - 1) / (max(line_range) - 1)
}
