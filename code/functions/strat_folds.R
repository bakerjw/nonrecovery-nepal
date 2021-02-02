# Function: strat_folds.R
# By: Sabine Loos
# Latest Update: 12.26.2019

# DESCRIPTION: Functions used to perform transforms on numeric variables

# FUNCTIONS: 
## logtr_std = logistic transform (to normalize) and standardize (mean = 0, std = 1)

# INPUTS:
## data = dataframe to add stratified folds to
## strat_cols = columns to stratify folds by

# OUTPUTS: 
## x = vector of transformed values

# VERSION HISTORY:
## The following code is derivative of the mapping_u5m_2017 code created by github user ihmeuw https://github.com/ihmeuw/ihme-modeling/tree/master/mapping_u5m_2017 
## Updated by Sabine Loos 10/15/2019


# functions ---------------------------------------------------------------
create_folds <- function(data,
                         n_folds=5,
                         strat_cols=NULL,
                         ...){
  
  ## make fold vectors stratifying by specified columns
  if(!is.null(strat_cols)){ # if you have columns to stratify by
    
    ## get different stratum (all the combinations of stratum columns)
    all_strat <- get_strat_combos(data=data, strat_cols=strat_cols)
    
    ## make a vector to identify folds and assign completely at random by strata
    fold_vec <- rep(NA, nrow(data))
    
    for(strat in 1:nrow(all_strat)){
      
      strata <- as.data.frame(all_strat[strat, ])
      colnames(strata) <- colnames(all_strat)
      
      ## get rows in current strata
      strat_rows <- get_strat_rows(data=data,
                                   strata=strata)
      
      ## assign fold numbers uniformly (with rounding)
      fold_s  <- cut(seq(1, length(strat_rows)),
                     breaks = n_folds, labels = 1:n_folds)
      fold_s <- as.numeric(as.character(fold_s))
      
      ## randomize the numbers within the strata
      fold_vec[strat_rows]  <- sample(fold_s)
    }
    
    ## check to make sure we got all rows
    if(sum(is.na(fold_vec) > 0)){
      message("Warning! Check why some data rows didn't get assigned to folds")
    }
    
  }else{ ## make folds across all data
    message("Add strat_cols")
  }
  
  
  ## and return a list containing
  ## 1) vector of folds
  ## 2) matrix containing all stratum (if exists)
  if(is.null(strat_cols)){
    return(list(folds   = NA,
                stratum = NA))
  }else{
    return(list(folds   = fold_vec,
                stratum = all_strat))
  }
}

get_strat_combos <- function(data=data,
                             strat_cols=strat_cols,
                             ...){
  
  ## this function creates all unique combinations of the columns we need to stratify over when making folds

  ## get all unique items from  each column
  unique_list <- list(NULL)
  for (i in 1:length(strat_cols)){
    unique_list[[i]] <- sort(unique(data[, strat_cols[i]]))
  }
  
  ## make a dataframe of all combinations and return it
  all_combos <- expand.grid(unique_list)
  colnames(all_combos) <- strat_cols
  return(all_combos)
  
}
get_strat_rows <- function(data=data,
                           strata,
                           ...){
  
  ## this function returns all the rows in a strata
  if(length(strata) < 1){
    message("Need to identify some strata!")
    stop()
  }
  
  ## loop through and intersect all rows we want
  good_rows <- data[, colnames(strata)[1] ] == strata[[1]]
  
  if(length(strata) > 1){
    for(c in 2:ncol(strata)){
      tmp_rows <- data[, colnames(strata)[c] ] == strata[[c]]
      good_rows <- good_rows * tmp_rows ## intersect them
    }
  }
  
  good_rows <- which(good_rows == 1)
  
  return(good_rows)
}
