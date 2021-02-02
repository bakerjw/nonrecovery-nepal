# Function: varselect_rf.R
# By: Sabine Loos
# Latest Update: 08.17.2020

# DESCRIPTION: Function to select out highly variables in random forest model that are less important than noise variable

# FUNCTIONS: 
## varselect_rf.R

# INPUTS:
## mod = random forest model (created using ranger)
## noise_var = name of column that is the noise variable

# OUTPUTS: 
## x = selected variables
## impurity = impurity values of selected variables

# VERSION HISTORY:
## written by Sabine Loos, 2020


# function -----------------------------------------------------------------
# select variables in RF that are more important than noise variable
varselect_RF <- function(mod, noise_var = "noise"){
  require(ranger); require(dplyr);
  # arrange importance
  imp = data.frame(value = names(ranger::importance(mod)), 
                   Impurity_corrected = ranger::importance(mod))%>%
    arrange(desc(Impurity_corrected))
  # find index for noise variable
  ind <- which(imp$value == noise_var)
  # select top features performing better than noise
  result <- as.character(imp$value[1:(ind-1)])
  
  # sum impurity_corrected
  sum = sum(imp$Impurity_corrected, na.rm = T)
  # calculate percentage of total impurity
  imp$impurity_perc <- imp$Impurity_corrected/sum
  impurity <- imp$impurity_perc[1:(ind-1)]
  return(list(x=result, impurity=impurity))
}
