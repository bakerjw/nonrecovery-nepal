# Function: varselect_corr.R
# By: Sabine Loos
# Latest Update: 08.04.2020

# DESCRIPTION: Function to select out highly collinear variables before modeling

# FUNCTIONS: 
## varselect_corr.R

# INPUTS:
## xvar = character vector of x variables to evaluate
## data = data frame holding all variables
## yvar = y variable to evaluate in log.fit
## corr_min = minimum correlation coefficient threshold, set to 0.75 by default
## folds_vec = vector of folds

# OUTPUTS: 
## x = selected variables
## xout.AIC = removed variables using AIC

# VERSION HISTORY:
## written by Sabine Loos, 2020

# varselect_ini (without VIF) ------------------------------------------------------------------
varselect_corr <- function(xvar, data, yvar, corr_min = 0.75, folds_vec = data$folds) {
  # examine correlations
  cor <- cor(data[,xvar], use = "pairwise.complete.obs")
  # create dataframe in descending order of correlation
  # set correlation threshold
  corr_thresh <- corr_min
  # remove lower triangle for pretransform variables
  corvars <- cor
  corvars[lower.tri(corvars, diag = T)] <- NA
  # select variables that have correlations larger than threshold
  ind <- which(corvars > corr_thresh | corvars < -corr_thresh, arr.ind=TRUE)
  if(nrow(ind)==0){ # if there are none greater than this correlation threshold then just return original values
    print("no highly correlated vars!!")
    xout.AIC <- NA;
  }else{
    # create dataframe of correlated variables
    corvars <- arrange(data.frame(var1 = rownames(corvars)[ind[,1]],var2 = rownames(corvars)[ind[,2]], cor = corvars[ind]), desc(abs(cor)))
    
    # check if we can run a full logistic model
    form <- as.formula(paste(yvar, paste(xvar, sep = "", collapse = " + "), sep = " ~ "))
    log.fit <- glm(formula = form, data = data, family = "binomial")
    # if any of the coefficients are zero. remove variables one by one based on AIC
    if(any(is.na(coef(log.fit)))){
      # add AIC columns and compute one by one AIC
      corvars$AIC2 <- corvars$AIC1 <- NA
      for (i in 1:nrow(corvars)) {
        corvars$AIC1[i] <- mean(cv.logm(dat_train = data,folds_vec = folds_vec,  y.var = yvar, x.var = corvars$var1[i], error.type = "AIC", family = "binomial")$errors, na.rm = T)
        corvars$AIC2[i] <- mean(cv.logm(dat_train = data,folds_vec = folds_vec,  y.var = yvar, x.var = corvars$var2[i], error.type = "AIC", family = "binomial")$errors, na.rm = T)
      }
      # remove variables with higher AIC
      xout.AIC <- NA
      maxAIC <- apply(corvars[,c("AIC1", "AIC2")], 1, which.max)
      while(any(abs(coef(log.fit)) > 10^3) | any(is.na(coef(log.fit))) | any(corvars$cor > corr_min)){ # until logistic regression converges or all correlation is under corr_min
        i=1
        if(maxAIC[i] ==1){
          xtoremove <- as.character(corvars[i,"var1"])
          # remove from list of X's
          xvar <- xvar[-which(xvar == xtoremove)]
          # remove all rows in corvars that contain that variable
          ind_aic <- c(which(corvars$var1 == xtoremove),which(corvars$var2 == xtoremove))
          corvars <- corvars[-ind_aic,]
        }else{
          xtoremove <- as.character(corvars[i,"var2"])
          # remove from list of X's
          xvar <- xvar[-which(xvar == xtoremove)]
          # remove all rows in corvars that contain that variable
          ind_aic <- c(which(corvars$var1 == xtoremove),which(corvars$var2 == xtoremove))
          corvars <- corvars[-ind_aic,]
        }
        xout.AIC <-append(xout.AIC, xtoremove)
        # initialize formula variables
        # formula
        form <- as.formula(paste(yvar, paste(xvar, sep = "", collapse = " + "), sep = " ~ "))
        suppressWarnings(log.fit <- glm(formula = form, data = data, family = "binomial"))
      }
      xout.AIC <- xout.AIC[-1]
    }else{
      print("able to fit logistic model without NA")
      xout.AIC <- NA;
    }
  } 
  
  # return list of final x, xout.aic, xout.vif
  return(list(x = xvar, xout.AIC = xout.AIC))
}
