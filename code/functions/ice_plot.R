# Script: ice.plot
# By: Sabine Loos
# Latest Update: 02.01.2021

# DESCRIPTION: Function to plot ICE plots nicely

# INPUTS:
## data = training data frame
## xvar = x variable character vector
## modeltype = character of model type (log or RF)
## model = model object
## truncate = T,F whether to truncate plots
## ylim = y limits of ICE plots
## ylab =  y label
## xlab = x label
## col_pal = color to plot
## transform_x = T,F whether to transform x

# OUTPUT:
## plot = ice plot ggplot object
## pd.dat = partial dependence data underlying ice plot

# VERSION HISTORY:
## written by Sabine Loos, February 2021


# function ----------------------------------------------------------------

ice.plot <- function(icevar,
                           data = dat_train45@data,xvar = xvar_fin,
                           yvar = "recon_binary",
                           modeltype, 
                           model,
                           truncate = F,
                           ylim = c(-0.5, 0.5),
                           ylab = paste0("Predicted prob. of nonrecovery ", round(center, digits = 2)),
                           xlab = icevar,
                           col_pal = "#11687E",
                           transform_x = F){
  require(iml)
  # create data frame with features
  features <- as.data.frame(data[ ,which(names(data) %in% xvar)])
  # create vector with responses
  response <- as.vector(data[,yvar])
  if(modeltype == "RF"){
    # create probability prediction vector
    predfun <- function(model, newdata){
      results <- as.data.frame(predict(model, data = newdata, type = "response")$predictions)[,2]}
    # create predictor object
    predictor <- Predictor$new(
      model = model,
      data = features,
      y = response, 
      predict.fun = predfun, 
      class = "probability"
    )
    # predict at data
    modpred <- predict(model, data = data, type = "response")
    data$prediction <- NA
    data$prediction <- modpred$predictions[,2]
  }else if(modeltype == "log"){
    predfun <- function(model, newdata){
      results <- as.vector((predict(model, newdata = newdata, type = "response", na.action = na.pass)))
    }
    
    
    # create predictor object
    predictor <- Predictor$new(
      model = model,
      data = features,
      y = response, 
      predict.fun = predfun, 
      class = "probability"
    )
    # predict at data
    modpred <- predict(model, data = data, type = "response")
    data$prediction <- NA
    data$prediction <- modpred
  }
  
  # var pd
  pd <- FeatureEffect$new(
    predictor = predictor,
    feature = icevar,
    method = "pdp+ice",
    grid.size = 50
  )
  
  # center at y value of minimum x feature
  center = min(features[,icevar])
  ind_center <-pd$results[,icevar]==center
  ind_center <- which(ind_center==T)
  center_y <- pd$results$.y.hat[ind_center[-1]]
  pd$center(center)
  pd.dat = pd$results
  
  # orig points predictions
  ogdat <- data.frame(x = data[,icevar], y = data$prediction-center_y)
  
  # add index per group
  # pd.dat.test <- pd.dat%>% group_by(.id) %>% mutate(index = row_number(.id)) %>% ungroup()
  prop = 0.1
  downsamp <- sample(1:max(pd.dat$.id, na.rm = T), size = round(0.15*max(pd.dat$.id, na.rm = T)))
  ind_downsamp <- which(pd.dat$.id %in% downsamp)
  pd.dat_ds<- pd.dat[-ind_downsamp,]
  pd.dat <- pd.dat_ds
  
  # baseplot
  iceplot <-  ggplot(pd.dat[pd.dat$.type == "ice",]) +
    # horizontal line
    geom_hline(aes(yintercept = 0), color = "gray") +
    # lines
    geom_line(data = pd.dat_ds,aes_string(x = icevar, y = ".y.hat", group = ".id"),color = col_pal[1], alpha = 0.01)+
    # points
    geom_point(data = ogdat, aes(x=x, y=y), col = col_pal[1],alpha = 0.20, size = 0.25)+
    # mean
    stat_summary_bin(aes_string(x = icevar, y = ".y.hat"),fun = mean, geom = "line", color = col_pal[1], size = 1)+
    labs(x = xlab, y = ylab)+
    plotThemeCoeff()
  
  # truncation and transformation
  top <- quantile(data[,icevar], 0.9899)
  bottom <- min(data[,icevar], na.rm = T)
  # pd.dat$lty = "solid"
  # pd.dat$lty[pd.dat[,icevar] >= top] = "dashed"
  
  if(transform_x == T){
    if(icevar == "popn2015_wp"){ #### change from m2 to km2
      m2tokm2 <- function() {
        function(x) format((x*1000*1000)/(100*100),digits = 2) 
      }
      if(truncate == T){
        iceplot = iceplot + 
          scale_x_continuous(expand = c(0,0), labels = m2tokm2(), trans='log10') +
          coord_cartesian(ylim = ylim,xlim = c(bottom, top), expand = F) + annotation_logticks(sides = "b")
      }else{
        iceplot = iceplot + 
          scale_x_continuous(expand = c(0,0), labels = m2tokm2(), trans='log10') +
          coord_cartesian(ylim = ylim, expand = F) + annotation_logticks(sides = "b")
      }
      
    }else{
      if(truncate == T){
        iceplot = iceplot + scale_x_continuous(expand = c(0,0), trans='log10')+coord_cartesian(ylim = ylim,xlim = c(bottom,top), expand = F) + annotation_logticks(sides = "b")
      }else{
        iceplot = iceplot + scale_x_continuous(expand = c(0,0), trans='log10')+coord_cartesian(ylim = ylim, expand = F) + annotation_logticks(sides = "b")
      }
    }
  }else{
    if(truncate == T){
      iceplot = iceplot + scale_x_continuous(expand = c(0,0))+coord_cartesian(ylim = ylim, xlim = c(bottom,top),expand = F)
    }else{
      iceplot = iceplot + scale_x_continuous(expand = c(0,0))+coord_cartesian(ylim = ylim, expand = F)
    }
  }
  
  return(list(plot = iceplot, data = pd.dat))
}