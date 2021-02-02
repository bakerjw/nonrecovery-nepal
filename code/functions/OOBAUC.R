# Function: OOBAUC.R
# By: Sabine Loos
# Latest Update: 08.11.2020

# DESCRIPTION: Function to calculate the out of bag area under the curve from a ranger model

# FUNCTIONS: 
## OOBAUC

# INPUTS:
## mod = random forest model (created using ranger)
## data = data used to train the model
## y = y variable

# OUTPUTS: 
## auc_avg = average area under the curve over all out of bag samples

# VERSION HISTORY:
## written by Sabine Loos, 2020


# function -----------------------------------------------------------------
OOBAUC <- function(mod, data, y){
  require(matrixStats)
  require(ROCR)
  if(is.null(mod$inbag.counts)){
    stop("ranger model has to be trained with 'keep.inbag = TRUE'")
  }
  # extract inbag counts
  inbag = do.call(cbind, mod$inbag.counts)
  # predict over all trees
  preds = predict(mod, data = data, predict.all = T, type = "response") # from predict.ranger: Return individual predictions for each tree instead of aggregated 
  # predictions for all trees. Return a matrix (sample x tree) for classification and regression,
  # Returns a 3d array for probability estimation (sample x class x tree).
  
  # reduce preds to the probability of second class.
  ind <- which(mod$forest$levels=="1")
  preds <- preds$predictions[,ind,]
  
  # calculate using inbag and out of bag predictions
  preds_inbag <- preds * ((inbag == 0) * 1)
  prob_array <- matrixStats::rowCumsums(preds_inbag) * (1 / rowCumsums((inbag == 0) * 1)) # divide by the number of observations that are out of bag
  
  # calculate OOB AUB
  if(mean(prob_array[data[,y.var]== 0,1], na.rm = T)>mean(prob_array[data[,y.var]== 1,1], na.rm = T)){
    pred_array <- apply(prob_array, 2, function(x) prediction(predictions = x, labels = data[,y.var], label.ordering = c("1","0")))
  }else{
    pred_array <- apply(prob_array, 2, function(x) prediction(predictions = x, labels = data[,y.var], label.ordering = c("0","1")))
  }
  auclist <- lapply(pred_array, function(x) performance(prediction.obj = x, measure = "auc"))
  auc_avg <- mean(unlist(lapply(auclist, function(x) attr(x, "y.value"))), na.rm = T)
  
  return(auc_avg)
}
