# Get effect sizes for multilevel models (nlme functions only= lme, gls)

t_stat_effsize <- function(model, denDF) {
  fit <- summary(model)
  
  # Ensure that there are coefficients to extract
  if ("coefficients" %in% names(fit)) {
    predictor_names <- names(fit$coefficients$fixed)
    t_stat <- fit$tTable[, "t-value"]
    degrees_freedom <- denDF
    
    # Calculate effect size
    effect_size <- sqrt((t_stat^2) / (t_stat^2 + degrees_freedom))
    
    # Check if lengths match
    if (length(predictor_names) == length(effect_size)) {
      # Create a data frame
      results <- data.frame(Variable = predictor_names, Effect_Size = effect_size)
      print(results)
    } else {
      cat("Length of predictor names and effect sizes do not match.\n")
    }
  } else {
    cat("Model has no coefficients.\n")
  }
}

f_stat_effsize <- function(model, denDf) {
  # Perform ANOVA
  fit <- anova(model, type = "marginal")
  options(scipen=999) 
    F_stat <- fit$`F-value`
    df_numerator <- fit$`numDF`
    df_denominator <- denDf
    
    # Calculate eta squared for each predictor
    eta_squared <- (df_numerator / df_denominator) * F_stat / (1 + (df_numerator / df_denominator) * F_stat)
    
    # Create a data frame for predictors excluding the model intercept
    results <- data.frame(Variable = rownames(fit)[-1], Eta_Squared = eta_squared[-1])
    
    # Print the table
    print(results)
}

