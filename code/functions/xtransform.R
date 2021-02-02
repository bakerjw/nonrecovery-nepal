# Function: xtransform.R
# By: Sabine Loos
# Latest Update: 12.26.2019

# DESCRIPTION: Functions used to perform transforms on numeric variables

# FUNCTIONS: 
## logtr_std = logistic transform (to normalize) and standardize (mean = 0, std = 1)

# INPUTS:
## x = vector of values to be transformed

# OUTPUTS: 
## x = vector of transformed values

# VERSION HISTORY:
## written by Sabine Loos, December 2019


# Functions ---------------------------------------------------------------
# LOGTR_STD
# taking the log of each variable, to create a normal-ish distribution
## then standardize to have a mean of zero and s.dev of 1
logtr_std <- function(x){
  # handles NAs
  x[x!=0 & !is.na(x)] <- log10(x[x!=0 & !is.na(x)]);
  # if 0, add small amount
  x[x==0 & !is.na(x)] <- log10((x[x==0 & !is.na(x)]+0.00000001))
  
  # standardize to have mean 0 and unit norm
  x <- std(x)
  return(x)
}

# STD
## standardize - have mean of 0 and s.dev of 1
std <- function(x){
  # standardize to have mean 0 and s.dev of 1
  x <- (x - mean(x, na.rm = T))/sd(x, na.rm = T)
  return(x)
}

## for scaling
scale_01 <-function(x){
  dif = diff(range(x, na.rm = T))
  x = (x-min(x, na.rm = T))/diff(range(x, na.rm = T))
  return(x)
}
