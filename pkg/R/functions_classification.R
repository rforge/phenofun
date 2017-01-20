###########################################################################
# Functions for classification of seasons
# Date: 09.05.16
# Author: Ludwig Bothmann
###########################################################################


###################################
# revise_seasons()
###################################

#' Revise classified seasons to extract unique onset days of seasons
#' 
#' @param doys vector of given doys
#' @param seasons_pred vector with predicted seasons
#' @return List with revised time series of seasons and extracted unique season
#'  onset dates.
#' @export
revise_seasons <- function(doys,
                           seasons_pred){
  
  x <- cbind(doys,seasons_pred)
  
  groups <- vector(length=nrow(x))
  groups[1] <- 1
  for(i in 2:nrow(x)){
    
    if(x[i,2]==x[i-1,2]){
      groups[i] <- groups[i-1]
    }else{
      groups[i] <- groups[i-1]+1
    }
    
  }
  
  x <- cbind(x, groups)
  
  tab_groups <- table(groups)
  
  season_groups <- tapply(x[,2], groups, mean)
  
  y <- cbind(tab_groups, season_groups)
  
  # 	spring_found <- FALSE
  # 	summer_found <- FALSE
  # 	autumn_found <- FALSE
  # 	winter_found <- FALSE
  stop_loop <- FALSE
  season_current <- "winter"
  for(i in 1:nrow(y)){
    
    if(!stop_loop){
      if(y[i,2]==1 & y[i,1]>=3 & season_current=="winter"){
        # spring_found <- TRUE
        spring_onset_group <- i
        spring_onset_doy <- min(x[which(x[,3]==spring_onset_group),1])
        season_current <- "spring"
      }
      
      if(y[i,2]==2 & y[i,1]>=3 & season_current=="spring"){
        # summer_found <- TRUE
        summer_onset_group <- i
        summer_onset_doy <- min(x[which(x[,3]==summer_onset_group),1])
        season_current <- "summer"
      }
      
      if(y[i,2]==3 & y[i,1]>=3 & season_current=="summer"){
        # autumn_found <- TRUE
        autumn_onset_group <- i
        autumn_onset_doy <- min(x[which(x[,3]==autumn_onset_group),1])
        season_current <- "autumn"
      }
      
      if(y[i,2]==4 & y[i,1]>=3 & season_current=="autumn"){
        # winter_found <- TRUE
        winter_onset_group <- i
        winter_onset_doy <- min(x[which(x[,3]==winter_onset_group),1])
        season_current <- "winter"
        stop_loop <- TRUE
      }
    }
  }
  
  onset_doys <- c(spring_onset_doy,
                  summer_onset_doy,
                  autumn_onset_doy,
                  winter_onset_doy)
  
  names(onset_doys) <- c("spring_onset_doy",
                         "summer_onset_doy",
                         "autumn_onset_doy",
                         "winter_onset_doy")
  
  seasons_new <- c(rep(4,(spring_onset_doy-min(x[,1]))),
                   rep(1,(summer_onset_doy-spring_onset_doy)),
                   rep(2,(autumn_onset_doy-summer_onset_doy)),
                   rep(3,(winter_onset_doy-autumn_onset_doy)),
                   rep(4,(max(x[,1])-winter_onset_doy+1)))
  # seasons_new							
  doy_total <- min(x[,1]):max(x[,1])
  #x <- cbind(x, seasons_new)
  
  seasons_doys_new <- data.frame(doy_total,seasons_new)
  
  return(list(seasons_doys_new,
              onset_doys))
}
