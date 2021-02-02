# Script: variable_selection.R
# By: Sabine Loos
# Latest Update: 02.01.2021

# DESCRIPTION: The script to automatically select variables to predict non-recovery 
## Method: Bootstrap multiple samples of the training data, run variable selection, and record variables that are selected

# DEPENDENCIES:
## xtransform.R
## strat_folds.R

# VERSION HISTORY:
## written by Sabine Loos, February 2021

# Packages and functions -----------------------------------------------------
# remove all existing files
rm(list = ls())
# load packages
packages <- c('glmnet', "ranger","pROC", "reshape2", "foreach", "future", "doFuture", "sp")
for (pkg in packages) {
  require(pkg, character.only = T)
}
# load functions
functions <- c('xtransform', 'strat_folds', 'varselect_corr', 'rf_train', 'OOBAUC', 'varselect_RF')
# functions <- c('create_folds', 'nscore', 'logtr', 'PlottingFunctions', 'cv', 'varselect_ini', "OOBCurve", "varselect_RF")
for (fn in functions) {
  source(file = paste0("code/functions/", fn, ".R"))
}
# set date
date <- format(Sys.Date(), "%m%d%y")

# remove variables from environment
rm(packages, pkg, functions, fn)

# Load data ---------------------------------------------------------------
# Read in spatial (sp) dataframe to use for building model. Data has column for y (dependent variable) and all other columns are x (ind. variable)
sp.xydat <- readRDS(file = "C:/Users/scloo/Documents/Research Codes/nonrecovery-nepal/data/in/TAF_11dist.rds")


# Prep x data -------------------------------------------------------------
# All x data is standardized. Some skewed variables are log transformed
## X variables that are log transformed, then standardizeed
x_logstd <- c(
  # remoteness
  "remoteMunic",
  # geography
  "popn2015_wp", 
  # census
  "n_tot", "p_wagework",  "p_thatchroof", "pop_dens")

## X variables that are standardized only
x_std <- c(
  # vegetation
  "trcvr_buff", 
  # geography
  "slopedeg", "MMI_usgs", 
  # hazards groups
  "lndsld_HZ_EQ_mr", "lndsld_HZ_rf_mr", 
  # wealth index
  "IWI_mn_2011", 
  # food poverty
  "FII_P2",
  # census
  "p_ag_work", "avg_age", "p_fhh", "p_toilet", "p_Dalit", "p_femaleworker",  "p_maleworker", "sex_ratio", "p_literate", "p_elect_light", 
  # census - communication
  "p_mobph",  "p_radio", "p_tv", 
  # material
  "p_mudbrick",  "p_mothtongue", "p_notdisabled", "p_tap",  
  # precipitation
  "msn_prcdiff_2015", "dry_prcdiff_2015" 
)

## Transform variables
### Apply log_std function to x_logstd variables
tr_dat <- data.frame(apply(sp.xydat@data[,x_logstd],2, logtr_std))
names(tr_dat) <- paste0(names(tr_dat), "_tr") # rename transformed variables to add _tr at end
sp.xydat <- maptools::spCbind(sp.xydat, tr_dat) # bind to original dataframe
### Apply std function to x_std variables
tr_dat <- data.frame(apply(sp.xydat@data[,x_std],2, std))
names(tr_dat) <- paste0(names(tr_dat), "_tr")
sp.xydat <- maptools::spCbind(sp.xydat, tr_dat)

# Add normal random noise variable (for random forest variable selection)
sp.xydat$noise <- rnorm(nrow(sp.xydat))

# set X variables for model
x <- c(paste0(x_logstd, "_tr"), paste0(x_std, "_tr"), "noise")

# Training and test set ---------------------------------------------------
## Model for only damaged buildings by using only data for damaged buildings (damage_binary = 1)
sp.xydat_dmg <- sp.xydat[which(sp.xydat$damage_binary==1),]

## Stratify data by y variable (recon_binary)
set.seed(930)
sp.xydat_dmg$folds <- create_folds(data = sp.xydat_dmg@data,
                              n_folds = 6, # five for training, one for test
                              strat_cols = c("recon_binary"))$folds
# save fold 3 as dat_test
sp.dattest <- sp.xydat_dmg[sp.xydat_dmg$folds == 3,]
sp.dattrain <- sp.xydat_dmg[sp.xydat_dmg$folds != 3,]

# make sure none of the variables are correlated over 0.75
x.sel <- varselect_corr(xvar = x, data = sp.dattrain@data, yvar = "recon_binary", folds_vec = sp.dattrain$folds, corr_min = 0.75)
x <- x.sel$x

# Initialize variables for bootstrap variable selection --------------------------------------------
## To select variables, we will run 1000 models with 1000 different bootstrapped samples of the training data
## Here we do this for a random forest and logistic regression.
## For the random forest, we select only those variables that have greater "importance" than the artificial noise variable (created above). 
### The importance metrice is the impurity_corrected measure in the ranger package.
## The variable reduction method is lasso regularization for the logistic regression.


# Define number of model runs
nruns = 1000

# Generate a vector of random seeds to pull from for each run
## Set script seed with a purely random number regurgitated straight from my brain
set.seed(2124)
##  create vector of seeds to use for each run
seeds <- sample(5000,size = (nruns),replace=F) 

# Initialize data frame for recording training error
error.train <- data.frame(matrix(nrow = 1, ncol = 2))
names(error.train) <- c("log", "RF")

# Initialize list of dataframes for coefficients
coef_avg_LOG <- data.frame(matrix(ncol = (length(x)+1), nrow = (1)))
names(coef_avg_LOG) <- append("(Intercept)", x)

coef_avg_RF <- coef_avg_LOG

# Set bootstrapped samples to pull each run. Each row is a row in the training data, Each column is a run.
bs_samp <- data.frame(matrix(ncol = nruns, nrow = nrow(sp.dattrain)))
names(bs_samp) <- paste0("run", 1:nruns)
for (run in 1:nruns) {
  # print(run)
  set.seed(seeds[run])
  # create sample for run
  # bootstrap sample
  bs_samp[,run] <- sample(nrow(sp.dattrain), nrow(sp.dattrain), replace = T)
}

# Set initial parameters to use for RF using full training dataset
y.var <- "recon_binary"
form <- as.formula(paste(y.var, paste(x, sep = "", collapse = " + "), sep = " ~ "))

# initial parameters when with impurity calculation
tuneparams <- rf_train(x.var = x, dattrain = sp.dattrain@data, impurity = T, form = form)
bestparams_imp <- tuneparams$bestparams
# model with bestparams for impurity calc
mod.imp <- ranger(
  formula = form,
  data = sp.dattrain@data,
  num.trees = length(x) * 10,
  mtry = bestparams_imp$mtry,
  min.node.size = bestparams_imp$min.node.size,
  replace = bestparams_imp$replace,
  sample.fraction = bestparams_imp$sample.fraction,
  probability = T,
  importance = 'impurity_corrected',
  seed = 523
)

# initial parameters for prediction, fewer variables
var_RF <- varselect_RF(mod = mod.imp)
# regrow with selected RF vars
x.var <- var_RF$x
form <- as.formula(paste(y.var, paste(x.var, sep = "", collapse = " + "), sep = " ~ "))
tuneparams <- rf_train(x.var = x.var, dattrain = sp.dattrain@data, impurity = F, form = form)
bestparams_pred <- tuneparams$bestparams

# Run in parallel ---------------------------------------------------------
# We do all of this in parallel, but still takes a few hours to run
## initialize parallelization
registerDoFuture()
ncores=parallel::detectCores()-1
cl<- parallel::makeCluster(ncores)
future::plan(tweak(cluster, workers = cl))
## combine function (rbind across all lists)
combine <- function(x, ...) {  
  mapply(rbind, x, ..., SIMPLIFY = FALSE)
}

# Run in parallel
# start time
t1 <- Sys.time()
t1
# run (takes about 1 hour)
BS_varselect <- foreach (run=1:nruns,
                         .combine = combine, # function to combine, self defined above
                         .multicombine = T, # combining more than 2 arguments
                         .init = list(data.frame(), data.frame(), data.frame()),
                         .errorhandling = "pass" , # remove error runs
                         .export = c("bs_samp", "coef_avg_LOG", "coef_avg_RF", "sp.dattrain", "error.train", "x", "create_folds", "varselect_RF", "bestparams_imp", "bestparams_pred", "rf_train"),
                         .verbose = T) %dopar% {
                           # select dat_run from bs_samp
                           dat_run <- sp.dattrain[bs_samp[,run],]
                           # define folds 
                           set.seed(seeds[run])
                           nfolds = 5
                           dat_run$folds <- create_folds(data = dat_run@data,
                                                         n_folds = nfolds,
                                                         strat_cols = c("recon_binary"))$folds
                           #---------- lasso-------------
                           # use selected x as model variables
                           y.var <- "recon_binary"
                           x.var <- x[-which(x == "noise")]
                           # specify inputs into glmnet
                           form <- as.formula(paste(y.var, paste(x.var, sep = "", collapse = " + "), sep = " ~ "))
                           x.mat <- model.matrix(form,dat_run@data)
                           y <- dat_run@data[rownames(dat_run@data) %in% rownames(x.mat), y.var]
                           # create fit
                           mod.las = cv.glmnet(x.mat,y, type.measure = "auc", 
                                                      family = "binomial",
                                                      nfolds = nfolds, 
                                                      foldid = dat_run@data$folds[rownames(dat_run@data) %in% rownames(x.mat)]) # specifying id of folds selected earlier
                           # save lasso data
                           las.dat <- data.frame(log(mod.las$lambda),mod.las$lambda, mod.las$cvm)
                           # coefficients for 1.se
                           s_mod.las = mod.las$lambda.1se
                           # select variables 
                           x.var <- c(names(which(abs(coef(mod.las, s = s_mod.las)[,1])>0)))
                           x.var <- x.var[-1] # remove intercept
                           # save CV errors
                           ind_1se <- which(las.dat$mod.las.lambda==s_mod.las)
                           error.train$log[1] <- las.dat$mod.las.cvm[ind_1se]
                           # save coefficients
                           coef <- data.frame(var = as.character(rownames(coef(mod.las, s = s_mod.las))), 
                                              coef = as.numeric(coef(mod.las, s = s_mod.las)))
                           coef <- coef[-2,] # removing intercept
                           coef$coef[coef$coef==0] <- NA # replacing 0 coefficients with NA
                           coef_avg_LOG[1,match(coef$var, names(coef_avg_LOG))] <- coef$coef
                           
                           #-----------RF-----------
                           y.var <- "recon_binary"
                           x.var <- x
                           # specify inputs into glmnet
                           form <- as.formula(paste(y.var, paste(x.var, sep = "", collapse = " + "), sep = " ~ "))
                           # RF model for impurity calculation (using impurity corrected bestparams)
                           mod.RF <- ranger(
                             formula = form,
                             data = dat_run@data,
                             num.trees = length(x.var) * 10,
                             mtry = bestparams_imp$mtry,
                             min.node.size = bestparams_imp$min.node.size,
                             replace = bestparams_imp$replace,
                             sample.fraction = bestparams_imp$sample.fraction,
                             probability = T,
                             importance = 'impurity_corrected',
                             seed = 523
                           )
                           # select RF vars
                           var_RF <- varselect_RF(mod = mod.RF)
                           
                           # regrow RF with selected RF vars
                           x.var <- var_RF$x
                           form <- as.formula(paste(y.var, paste(x.var, sep = "", collapse = " + "), sep = " ~ "))
                           # select best params for general rf model
                           if(bestparams_pred$mtry > length(x.var)){ # if there are more mtry than there are selected variables, retrain to get new params
                             bestparams_pred <- rf_train(x.var = x.var, dattrain = dat_run@data, impurity = F,form = form)$bestparams
                           }
                           # retrain model to record training error (can't be done with impurity model above)
                           mod.RF <- ranger(
                             formula = form,
                             data = dat_run@data,
                             num.trees = length(x.var) * 10,
                             mtry = bestparams_pred$mtry,
                             min.node.size = bestparams_pred$min.node.size,
                             replace = bestparams_pred$replace,
                             sample.fraction = bestparams_pred$sample.fraction,
                             keep.inbag = T,
                             probability = T,
                             seed = 523
                           ) 
                           # save CV errors
                           error.train$RF[1] <- OOBAUC(mod = mod.RF, data = dat_run@data, y = y.var) #AUC
                           # save impurity_corrected/importance
                           coef_avg_RF[1,match(var_RF$x, names(coef_avg_RF))] <- var_RF$impurity
                           #-----------save-----------
                           list(error.train = error.train, coef_avg_LOG = coef_avg_LOG, coef_avg_RF = coef_avg_RF)
                         }
run.time <- Sys.time() - t1 # 1.13 hours
names(BS_varselect) <- c("error.train", "coef_avg_LOG","coef_avg_RF")


# Save to data out --------------------------------------------------------
save(BS_varselect, file = "data/out/varselect_nrun_1000.RData")
