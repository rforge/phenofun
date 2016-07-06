###########################################################################
# Functions for supervised classification method
# Date: 01.07.16
# Author: Ludwig Bothmann
###########################################################################

#' Function for eigenanalysis and supervised classification
#' 
#' This function allows to carry out an eigenanalysis of given webcam data
#'  and a supervised classification of DOYs with respect to seasons. 
#'  Example code can be downloaded at 
#'  \url{http://bothmann.userweb.mwn.de/dissertation.html}.
#'  
#' @param folder_results Folder where the results will be saved
#' @param folder_data Folder where the data is saved
#' @param name_of_analysis Name of the specific analysis for a unique folder
#' @param lists_files Vector of file names of images
#' @param do_eigenanalysis If \code{TRUE} (default), eigenanalysis is carried 
#'  out
#' @param do_classification If \code{TRUE} (default), supervised classification 
#'  is carried out
#' @param do_classification_newyear If \code{TRUE}, supervised 
#'  classification of a new/test year is carried out, default is \code{FALSE}
#' @param do_masking If \code{TRUE}, images are masked with a given 
#'  ROI as first step is carried out, default is \code{FALSE}
#' @param load_eigen If \code{TRUE}, results of eigenanalysis are 
#'  loaded, default is \code{FALSE}
#' @param show_eigen If \code{TRUE}, eigenimages are save, default is 
#'  \code{FALSE}
#' @param varimax_rotation If \code{TRUE}, principal components are rotated with
#'  varimax method, default is \code{FALSE}
#' @param sum1 If \code{TRUE}, images are normed to sum=1, default is 
#'  \code{FALSE}
#' @param norm_it If \code{TRUE} (default), images are normed before the 
#'  analysis
#' @param norm_type Type of norming
#' @param colorspace Color space (rgb, hsv, etc.)
#' @param total_var If \code{TRUE} (default), total variance is computed in 
#'  eigenanalysis
#' @param col_in Color channels to be read in, default \code{1:3} corresponds
#'  to red, blue and green
#' @param colormode The color mode
#' @param filename_mask File name of mask / ROI
#' @param pfad_eigen Path of results of eigenanalysis to be loaded
#' @param fact Weighting factor for sum and diff of mean and eigenimages, 
#'  default is \code{NULL} and then quantiles of the scores are used as weights
#' @param x vector of x-coordinates of analyzed pixels
#' @param y vector of y-coordinates of analyzed pixels
#' @param training_year The training year
#' @param test_year The test year
#' @param season_list List specifying the true seasons
#' @param n_pc Number of principal components / eigenimages
#' @param year Vector of year for each image
#' @param doy Vector of DOYs for each image
#' @param complete_year If \code{TRUE} (default), the complete year is analysed,
#'  else the first \code{n_images_try} are analyzed (only for testing)
#' @param n_images_try See complete_year
#' @param seasons_newyear Vector with true seasons of the test year
#' @param mask Do not edit, mask is loaded, if needed
#' @return An entire supervised classification analysis is carried out.
#'  All results are saved on the disc, the necessary folders are created.
#' @export
#' @import MASS
sup_class <- function(folder_results=paste0(getwd(),"/"),
                      folder_data=paste0(getwd(),"/"),
                      name_of_analysis=substr(as.character(Sys.time()),1,10),
                      lists_files=lists_files,
                      do_eigenanalysis=TRUE,
                      do_classification=TRUE,
                      do_classification_newyear=FALSE,
                      do_masking=FALSE,
                      load_eigen=FALSE,
                      show_eigen=FALSE,
                      varimax_rotation=FALSE,
                      sum1=FALSE,
                      norm_it=TRUE,
                      norm_type="mean0",
                      colorspace="rgb",
                      total_var=TRUE,
                      col_in=1:3,
                      colormode="Color",
                      filename_mask=NULL,
                      pfad_eigen=NULL,
                      fact=NULL,
                      x=NULL,
                      y=NULL,
                      training_year,
                      test_year,
                      season_list,
                      n_pc=12,
                      year,
                      doy,
                      complete_year=TRUE,
                      n_images_try=10,
                      seasons_newyear,
                      mask=NULL
                      
){
  
  # Create directories
  dir.create(paste0(folder_results,name_of_analysis,"/eigenimages/images/"), recursive = TRUE)
  dir.create(paste0(folder_results,name_of_analysis,"/classification_seasons/"), recursive = TRUE)
  
  file_scores <- paste0(folder_results,name_of_analysis,"/eigenimages/eigenimages.RData")
  pfad_plots <- paste0(folder_results,name_of_analysis,"/classification_seasons/")
  folder_scores_eigen <- paste0(folder_results,name_of_analysis,"/eigenimages/images/")
  
  if(!do_classification & do_classification_newyear){
    stop("do_classification=TRUE needed, if do_classification_newyear=TRUE")
  }
  
  if(do_masking & is.null(filename_mask)) stop("File name of mask is missing. \n")
  
  if(load_eigen & is.null(pfad_eigen)) stop("File name eigenimages is missing. \n")
  
  files_date <- data.frame(file=lists_files,
                           year,
                           doy,
                           frame=seq_len(length(lists_files)))
  
  if(do_classification_newyear){
    file_scores_newyear <- file_scores
  }else{
    file_scores_newyear <- NULL
  }
  
  if(do_classification_newyear){
    # Only year "test_year":
    which_images <- files_date$frame[which(files_date$year%in%test_year)]
  }else{
    # Only year "training_year":
    which_images <- files_date$frame[which(files_date$year%in%training_year)]
  }
  
  if(complete_year){
    
    if(do_classification_newyear){
      # Only year "test_year":
      which_images <- files_date$frame[which(files_date$year%in%test_year)]
    }else{
      # Only year "training_year":
      which_images <- files_date$frame[which(files_date$year%in%training_year)]
    }
    
  }else{
    
    # from 1 to n_images_try
    which_images <- seq_len(n_images_try)
    
  }
  
  # cut files_date
  files_date <- files_date[which_images,]
  
  if(colormode=="Color"){
    col_out <- 1:3	
  }else{
    col_out <- 1
  }
  
  # Number of images
  n_images <- length(which_images)
  
  #############################################################################
  # Compute eigenimages
  #############################################################################
  
  if(do_eigenanalysis){
    
    cat(as.character(Sys.time()),"Start read images \n")
    
    # Read images
    images <- read_images(colormode=colormode,
                          folder=folder_data,
                          file_names=lists_files,
                          which_images=which_images,
                          x=x,
                          y=y,
                          col_out=col_out,
                          col_in=col_in,
                          sum1=sum1,
                          norm_it=norm_it,
                          norm_type=norm_type,
                          colorspace=colorspace)
    
    cat(as.character(Sys.time()),"End read images \n")
    
    
    # Optionally mask images
    if(do_masking){
      
      load(filename_mask)
      
      images <- mask_img(images,mask=mask)
      
    }
    
    # Only compute eigenimages if not loaded
    if(!load_eigen){
      
      # Compute eigenimages and eigenvalues
      eigen_img <- eigen_images(images, 
                                n_pc=n_pc,
                                varimax_rotation=varimax_rotation,
                                total_var=total_var)	
    }else{
      
      files_date_backup <- files_date
      
      # Load eigenimages
      load(pfad_eigen) 
      
      files_date <- files_date_backup
    }
    
    #############################################################################
    # Compute scores and save results
    #############################################################################
    
    # Compute scores on eigenimages and eigenvalues
    scores <- scores_images(images, eigen_img$eigenimages, center=TRUE)
    
    # Mean image
    images_flat <- array(images, dim=c(prod(head(dim(images), -1)), 
                                       tail(dim(images), 1)))
    images_flat_mean <- rowMeans(images_flat)
    
    # Settings
    settings <- list(colormode=colormode,
                     n_pc=n_pc,
                     varimax_rotation=varimax_rotation,
                     complete_year=TRUE,
                     test_year=test_year,
                     training_year=training_year,
                     which_images=which_images,
                     x=x,
                     y=y,
                     sum1=sum1,
                     load_eigen=load_eigen,
                     pfad_eigen=pfad_eigen,
                     norm_it=norm_it,
                     colorspace=colorspace,
                     norm_type=norm_type,
                     col_in=col_in,
                     do_masking=do_masking,
                     filename_mask=filename_mask)
    
    # Save results
    if(load_eigen){
      save(scores, images_flat_mean, settings, files_date,
           file = file_scores)
    }else{
      save(eigen_img, scores, images_flat_mean, settings, files_date,
           file = file_scores)
    }
    
    cat(as.character(Sys.time()),": Results of eigenanalysis saved \n")
    
    #############################################################################
    # Plot and save eigenimages and score time series
    #############################################################################
    
    if(!do_classification_newyear){
      
      load(file_scores)
      
      # Only, if eigenimages were computed before
      if(!load_eigen){
        
        # Ratio of variance explained
        explained_var <- round((eigen_img$svd_values^2/(n_images-1))/eigen_img$total_variance * 100,2)
        
        # Title for plots of scores
        main_plot <- paste0(1:nrow(scores), ". eigenimage: ", explained_var,"% variance explained")
        
        # Save sums and diffs of mean image and weghted eigenimage
        display_eigen(eigenimages = eigen_img$eigenimages,
                      colormode = settings$colormode,
                      n_pc = nrow(scores),
                      images_flat_mean = images_flat_mean,
                      fact = fact, #100,#1e-04,#200,
                      do_sum_diff = TRUE,
                      sum1 = settings$sum1, # FALSE
                      save_it=TRUE,
                      save_file=folder_scores_eigen,
                      show_eigen=show_eigen, # FALSE
                      colorspace = settings$colorspace,
                      scores=scores)
      }else{
        
        main_plot <- NULL
      }	
      
      # Plot scores
      for(sep_plots in c(TRUE,FALSE)){
        if(sep_plots){
          cex.lab <- 2
          cex.axis <- 2
          cex.main <- 2
        }else{
          cex.lab <- 1
          cex.axis <- 1
          cex.main <- 1
        }
        display_scores(scores = scores,
                       year = files_date$year,
                       doy = files_date$doy, 
                       complete_year = settings$complete_year,
                       which_year = settings$training_year,
                       which_images = settings$which_images,
                       n_show=settings$n_pc,
                       main=main_plot,
                       save_it=TRUE,
                       save_file=folder_scores_eigen,
                       sep_plots=sep_plots,
                       plot_lowess=FALSE,
                       cex.lab=cex.lab,
                       cex.axis=cex.axis,
                       cex.main=cex.main)
      }
      
      
      
      #############################################################################
      # Screeplots
      #############################################################################
      
      # Only, if eigenimages were computed before
      if(!load_eigen){
        
        pdf(paste0(folder_scores_eigen,"screeplots.pdf"), width=12, height=8)
        par(mfrow=c(1,2))
        
        # Variance explained per eigenimage
        plot(seq_len(n_pc), 
             (eigen_img$svd_values^2/(n_images-1))/eigen_img$total_variance, 
             type="b",
             main="Screeplot eigenimages",
             ylab="Variance explained",
             xlab="Index eigenimage")
        abline(h=.05)
        
        # Cumulated variance explained
        plot(seq_len(n_pc), 
             cumsum((eigen_img$svd_values^2/(n_images-1))/eigen_img$total_variance), 
             type="b",
             main="Cumulated variance explained eigenimages",
             ylab="Cumulated variance explained",
             xlab="Index eigenimage")
        abline(h=.95)
        
        dev.off()
      }
    }
  }
  
  #############################################################################
  # Classification
  #############################################################################
  
  if(do_classification){
    
    if(!file.exists(pfad_plots)) dir.create(pfad_plots, recursive=TRUE)
    
    if(do_classification_newyear){
      
      # Load scores and files_date of training year
      load(pfad_eigen) 
    }else{
      
      # Load scores and files_date of training year
      load(file_scores)
    }
    
    class_data <- data.frame(file=files_date$file,
                             doy=files_date$doy)
    
    doy <- files_date$doy

    # Season categories
    class_data$season <- rep(0,nrow(files_date))
    class_data$season[which(is.element(doy,season_list$spring))] <- 1#3#1
    class_data$season[which(is.element(doy,season_list$summer))] <- 2
    class_data$season[which(is.element(doy,season_list$autumn))] <- 3
    class_data$season[which(is.element(doy,season_list$winter))] <- 4
    
    # Add scores 
    class_data <- cbind(class_data,t(scores))
    
    # Delte file name and doy for classifikations data set
    class_season <- class_data[,-(1:2)]
    
    # Season as factor
    class_season$season <- factor(class_season$season, levels = 1:4)
    
    # Plot of first 4 scores
    pdf(paste0(pfad_plots,"descr_season.pdf"),width=12, height=8)
    plot(class_season[,2:5], col=(class_season$season),pch=19,cex=1)
    dev.off()
    
    pch_seasons <- ifelse(class_season$season==1, 15,
                          ifelse(class_season$season==2, 17,
                                 ifelse(class_season$season==3, 18,19)))
    col_seasons <- ifelse(class_season$season==1, 3,
                          ifelse(class_season$season==2, 1,
                                 ifelse(class_season$season==3, 2,4)))
    
    jpeg(paste0(pfad_plots,"descr_season_pc1_2.jpg"),width=10, height=6,
         units="in", res=150, quality=100)
    par(mfrow=c(1,1), mar=c(5,5,4,1)+.1)
    plot(class_season[,2:3], col=col_seasons,pch=pch_seasons,cex=1.3,
         xlab="Eigenimage 1", ylab="Eigenimage 2",
         main="Scores on first two eigenimages",
         cex.lab=2, cex.main=2, cex.axis=2)
    legend("topleft", c("Spring", "Summer","Autumn","Winter"),
           col=c(3,1,2,4),
           pch=c(15,17,18,19),cex=1.5)
    dev.off()
    
    jpeg(paste0(pfad_plots,"descr_season_pc2_3.jpg"),width=12, height=8,
         units="in", res=150, quality=100)
    par(mfrow=c(1,1), mar=c(5,5,4,1)+.1)
    plot(class_season[,3:4], col=col_seasons,pch=pch_seasons,cex=1.3,
         xlab="Eigenimage 2", ylab="Eigenimage 3",
         main="Scores on second and third eigenimages",
         cex.lab=2, cex.main=2, cex.axis=2)
    legend("topright", c("Spring", "Summer","Autumn","Winter"),
           col=c(3,1,2,4),
           pch=c(15,17,18,19),cex=1.5)
    dev.off()
    
    # QDA
    qda2 <- qda(season ~ . ,data=class_season, CV=TRUE)
    # LOO-CV- prediction
    pred_class2 <- apply(qda2$posterior,1, which.max)#+1
    
    # Plot of classification
    pdf(paste0(pfad_plots,"cv_seasons.pdf"),width=12, height=8)
    par(mfrow=c(1,1))
    plot(class_data$doy,class_data$season,pch=19,ylim=c(0.5,4.9),
         xlab="DOY",ylab="season",
         main="Seasons (1-4=Spring-Winter)",axes=FALSE,
         cex.lab=1.5, cex.main=1.5)
    axis(1,at=seq(0,350,by=50),cex.axis=1.5)
    axis(2,at=c(1,2,3,4),cex.axis=1.5)
    lines(class_data$doy,pred_class2+.1,pch=8, type="b",col="orange")
    legend("topleft",c("Predicted Class LOO-CV", "True Class"),
           pch=c(8,19),cex=1.4,
           col=c("orange","black"))
    dev.off()
    
    # Ratio misclassified
    ratio_misclass <- sum(pred_class2!=class_data$season)/nrow(class_data) * 100
    cat("Ratio misclassified images seasons: ", ratio_misclass, "% \n")
    
    # Which are misclassified?
    which_falsch <- which(pred_class2!=class_data$season)
    doy_falsch <- files_date$doy[which_falsch]
    
    # Posteriors of wrong and right classes
    post_misclass <- round(cbind(qda2$posterior[which_falsch,],class_data$season[which_falsch]),2)
    
    cat("Posterior class probabilities for misclassified images seasons: \n ")
    print(post_misclass)
    
    
    #################################
    # Average per DOY
    #################################
    
    post_doy <- matrix(nrow=length(unique(doy)),ncol=4)
    rownames(post_doy) <- unique(doy)
    for(i in 1:4){
      post_doy[,i] <- tapply(qda2$posterior[,i],doy, mean)
    }
    
    # Season per DOY
    season_doy <- tapply(class_data$season, doy, mean)
    
    # Predicted class per DOY
    pred_class_doy <- apply(post_doy,1, which.max)#+1
    
    # Plot
    pdf(paste0(pfad_plots,"cv_seasons_doy.pdf"),width=12, height=8)
    par(mfrow=c(1,1))
    plot(rownames(post_doy),season_doy-.1,type="p",pch=19,ylim=c(0.5,5.3),
         xlab="DOY",ylab="season",
         main="Classification LOO-CV",axes=FALSE,
         cex.lab=2, cex.main=2)
    axis(1,at=seq(0,350,by=50), cex.axis=2)
    axis(2,at=c(1,2,3,4), cex.axis=2)
    lines(rownames(post_doy),pred_class_doy+.1,pch=8, type="b",col="orange")
    sd_falsch <- as.numeric(names(which(season_doy!=pred_class_doy)))
    points(sd_falsch,pred_class_doy[which(season_doy!=pred_class_doy)]+.1,
           col="blue")
    legend("topleft",c("Predicted Class", "True Class",
                       "Misclassified"),
           pch=c(8,19,1),cex=1.5,
           col=c("orange","black","blue"))
    legend("topright", c("1 = Spring","2 = Summer","3 = Autumn","4 = Winter"),
           cex=1.5)
    dev.off()
    
    ####################################################################
    # Find unique season onset DOYS
    ####################################################################
    
    seasons_revised <- revise_seasons(doys = as.numeric(rownames(post_doy)),
                                      seasons_pred=pred_class_doy)
    onset_dates <- seasons_revised[[2]]
    save(onset_dates,file = paste0(pfad_plots,"onset_dates.RData"))
    
    which_misclass <- which(season_doy!=pred_class_doy)
    
    cat("Misclassified DOYs seasons: \n ")
    print(names(which_misclass))
    
    cat("Posterior class probabilities for misclassified DOYs seasons: \n ")
    print(round(post_doy[which_misclass,],2))
    
    ratio_misclass_doy <- sum(season_doy!=pred_class_doy) / length(season_doy) * 100
    
    cat("Ratio misclassified per DOY seasons: ", ratio_misclass_doy, "% \n")
    
    ######################################################
    # Classification new year
    ######################################################
    
    if(do_classification_newyear){
      
      qda_oldyear_season <- qda(season ~ . ,data=class_season)
      
      # Load scores of test year on training year
      load(file_scores_newyear)
      
      # classification data set
      class_newyear <- data.frame(doy=files_date$doy)
      class_newyear <- cbind(class_newyear,t(scores))
      
      # Prediction
      pred_newyear <- predict(qda_oldyear_season, newdata=class_newyear[,-1])
      
      # Predicted class
      pred_class_newyear <- pred_newyear$class
      

      #############################################################
      # Average posterior probability per DOY
      #############################################################
      
      post_doy_newyear <- matrix(nrow=length(unique(files_date$doy)),ncol=4)
      rownames(post_doy_newyear) <- unique(files_date$doy)
      for(i in 1:4){
        post_doy_newyear[,i] <- tapply(pred_newyear$posterior[,i],files_date$doy, mean)
      }
      
      # Prediction on DOY level
      pred_class_newyear_doy <- apply(post_doy_newyear,1, which.max)#+1
      
      # Plot 
      pdf(paste0(pfad_plots,"newyear_seasons_doy.pdf"),width=12, height=8)
      par(mfrow=c(1,1))
      plot(rownames(post_doy_newyear),pred_class_newyear_doy,
           type="b",pch=8,
           ylim=c(0.5,5.3),col="orange",
           xlab="DOY",ylab="season",
           main="Classification test year",axes=FALSE,
           cex.main=2, cex.lab=2)
      axis(1,at=seq(0,350,by=50), cex.axis=2)
      axis(2,at=c(1,2,3,4), cex.axis=2)
      legend("topleft",c("Predicted class"),
             pch=8,cex=1.5,
             col="orange")
      legend("topright", c("1 = Spring","2 = Summer","3 = Autumn","4 = Winter"),
             cex=1.5)
      dev.off()
      
      ####################################################################
      # Find unique season onset DOYS
      ####################################################################
      
      seasons_revised <- revise_seasons(doys = as.numeric(rownames(post_doy_newyear)),
                                        seasons_pred=pred_class_newyear_doy)
      onset_dates <- seasons_revised[[2]]
      save(onset_dates,file = paste0(pfad_plots,"newyear_onset_dates.RData"))
      
      pdf(paste0(pfad_plots,"newyear_seasons_doy_revised.pdf"),width=12, height=8)
      par(mfrow=c(1,1))
      plot(seasons_revised[[1]]$doy_total,seasons_revised[[1]]$seasons_new+.1,
           type="b",pch=8,
           ylim=c(0.5,5.3),col="orange",
           xlab="DOY",ylab="season",
           main="Classification test year revised",axes=FALSE,
           cex.main=2, cex.lab=2)
      axis(1,at=seq(0,350,by=50), cex.axis=2)
      axis(2,at=c(1,2,3,4), cex.axis=2)
      lines(seasons_revised[[1]]$doy_total, seasons_newyear-.1,type="b",pch=19,
            col="black")
      doy_falsch <- seasons_revised[[1]]$doy_total[which(seasons_revised[[1]]$seasons_new!=seasons_newyear)]
      season_falsch <- seasons_revised[[1]]$seasons_new[which(seasons_revised[[1]]$seasons_new!=seasons_newyear)]
      points(doy_falsch,season_falsch+.1,
             col="blue")
      legend("topleft",c("Predicted class after revision","True class",
        "Misclassified"),
        pch=c(8,19,1),cex=1.5,
        col=c("orange","black","blue"))
      legend("topright", c("1 = Spring","2 = Summer","3 = Autumn","4 = Winter"),
             cex=1.5)
      dev.off()
      
    }
  }
}