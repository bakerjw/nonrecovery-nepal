# Function: rf_train.R
# By: Sabine Loos
# Latest Update: 02.21.2021

# DESCRIPTION: Function to train a random forest using ranger package. Uniform grid search.

# FUNCTIONS: 
## rf_train.R

# INPUTS:
## xvar = character vector of x variables to evaluate
## data = data frame holding all variables
## yvar = y variable to evaluate in log.fit
## corr_min = minimum correlation coefficient threshold, set to 0.75 by default
## folds_vec = vector of folds

# OUTPUTS: 
## x_sel = selected variables

# VERSION HISTORY:
## written by Sabine Loos, 2021


# create hyperparameter dataframe with all tuning parameters (for mtry and nodesize) in rf
rf_train <- function(x.var,
                     dattrain,
                     impurity = T,
                     form){
  require(ranger); require(dplyr)
  # Create grid of all combinations of hyperparams to tune (mtry, min.node.size, replace, sample.fraction)
  hyper_grid <- expand.grid(
    mtry = floor(length(x.var) * seq(0.15, 0.65, by = 0.1)),
    min.node.size = floor(nrow(dattrain) * seq(0.03,0.1, by = 0.01)),
    replace = c(T, F),
    sample.fraction = seq(0.5, 0.8, by = 0.1),
    rmse = NA
  )
  # record rmse for model ran with each of the hyperparameter combinations
  if(impurity == T){
    for (i in seq_len(nrow(hyper_grid))) {
    # fit for all parameters in hyper grid
    fit <- ranger(
      formula = form,
      data = dattrain,
      num.trees = length(x.var) * 10,
      mtry = hyper_grid$mtry[i],
      min.node.size = hyper_grid$min.node.size[i],
      replace = hyper_grid$replace[i],
      sample.fraction = hyper_grid$sample.fraction[i],
      importance = "impurity_corrected",
      probability = T,
      seed = 523
    )
    hyper_grid$rmse[i] <- sqrt(fit$prediction.error)
    }
  }else{
    for (i in seq_len(nrow(hyper_grid))) {
      # fit for all parameters in hyper grid
      fit <- ranger(
        formula = form,
        data = dattrain,
        num.trees = length(x.var) * 10,
        mtry = hyper_grid$mtry[i],
        min.node.size = hyper_grid$min.node.size[i],
        replace = hyper_grid$replace[i],
        sample.fraction = hyper_grid$sample.fraction[i],
        importance = "impurity_corrected",
        probability = T,
        seed = 523
      )
      hyper_grid$rmse[i] <- sqrt(fit$prediction.error)
    }
  }
  
  # select best parameters (lowest rmse)
  bestparams<- hyper_grid %>%
    arrange(rmse) %>% head(1)
  bestparams <- bestparams[,1:4]
  # return best params
  return(list(bestparams = bestparams, mod = fit))
}
