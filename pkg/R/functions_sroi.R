###########################################################################
# Functions for sROI method
# Date: 01.07.16
# Author: Ludwig Bothmann
###########################################################################

#' Function for sROI analysis
#' 
#' This function allows to carry out an entire sROI analysis for given 
#' webcam data
#' 
#' @param folder_results Folder where the results will be saved
#' @param folder_data Folder where the data is saved
#' @param name_of_analysis Name of the specific analysis for a unique folder
#' @param get_timeseries If \code{TRUE} (default), greenness time series of 
#'  pinpricks are extracted
#' @param correlate_timeseries If \code{TRUE} (default), correlation images 
#'  are computed
#' @param do_evaluaion If \code{TRUE} (default), correlation images and time
#'  series are saved
#' @param do_strucchange If \code{TRUE} (default), optimality criteria are
#'  computed and best mask is saved
#' @param color Name of color channel to be analyzed, typically \code{green} 
#'  (default)
#' @param main_camera Main for the output plots
#' @param n_pinpricks Number of random pinpricks
#' @param seed Seed for the random pinpricks
#' @param threshold_vec Vector of thresholds for the correlation images
#' @param a_vec Possible spring doys
#' @param b_vec Possible autumn doys (counted backwards from 31.12.)
#' @param lists_files Vector of file names of images
#' @param which_images Vector of indices of images to be analyzed
#' @param doy Vector of DOYs for each image
#' @param doy_c Vector of DOYs for each image, continouing over a change of year
#'  of more than one year is analyzed, else \code{doy}
#' @param which_images_background Index of background image
#' @export
sroi_analysis <- function(folder_results=paste0(getwd(),"/"),
                          folder_data=paste0(getwd(),"/"),
                          name_of_analysis=substr(as.character(Sys.time()),1,10),
                          get_timeseries=TRUE,
                          correlate_timeseries=TRUE,
                          do_evaluation=TRUE,
                          do_strucchange=TRUE,
                          color="green",
                          main_camera=NULL,
                          n_pinpricks=1,
                          seed=12345,
                          threshold_vec=.9,
                          a_vec = seq(1,150,by=1),
                          b_vec = seq(1,100,by=1),
                          lists_files,
                          which_images=seq_len(length(lists_files)),
                          doy,
                          doy_c=doy,
                          which_images_background=1){
  
  #################################################################
  # Steps:
  # 
  #	1. Set random pinpricks
  #   - Save pinpricks as RData (optional)
  # 	- Save pinpricks as jpg
  # 		
  #	2. Compute mean %greenness time series in pinpricks
  #		- Save as jpg + RData (optional)
  #		
  #	3. Compute correlation of all pixel's %greenness time series with the 
  #       pinpricks %greenness time series
  #		- Save correlation images (optional)
  #		
  #	4. Threshold correlation images to produce ROIs
  #		- Save ROIs as jpg + RData (optional)
  #		
  #	5. Compute mean %greenness time series in ROIs
  #		- Average on DOY level
  #		
  #	6. Compute optimality criteria for all pinpricks and threshold
  #	
  #	7. Evaluation
  #		- Which combination of pinprick and threshold is optimal 
  #       with respect to OC1/OC2?
  #		- Plot time series of optimal sROI
  #		- Save sROI
  #################################################################
  
  set.seed(seed)
  files_date <- data.frame(doy=doy, doy_c=doy_c)[which_images,]
  
  # Create directories
  dir.create(paste0(folder_results,name_of_analysis,"/corr_images/"), recursive = TRUE)
  dir.create(paste0(folder_results,name_of_analysis,"/mask_timeseries/"), recursive = TRUE)
  dir.create(paste0(folder_results,name_of_analysis,"/masks/"), recursive = TRUE)
  
  #################################################################
  #	1. Set random pinpricks
  #   - Save pinpricks as RData (optional)
  # 	- Save pinpricks as jpg
  #################################################################
  
  # Set parameter for background image
  x <- 1285:2560
  y <- 1:960
  col_out <- 1:3
  col_in <- 1:3
  
  # Read background image
  images <- read_images(colormode="Color",
                        folder=folder_data,
                        file_names=lists_files,
                        which_images=which_images_background,
                        x=x,
                        y=y,
                        col_out=col_out,
                        col_in=col_in,
                        sum1=FALSE,
                        norm_it=FALSE,
                        colorspace="rgb")
  
  
  # Background image as gray image
  imgn <- normalize(rgbImage(red = images[,,1,],green = images[,,1,],blue = images[,,1,]))
  
  # # Show image
  # display(imgn)
  # => If you move with the mouse over the image, pixel coordinates are shown
  
  # Darker
  imgn2 <- imgn/3
  # display(imgn2)
  
  # Color for pinpricks (1=red, 2=green, 3=blue)
  col_pinprick <- 2
  
  
  ################################
  # Random pinpricks
  ################################
  
  x_start <- sample(head(seq_len(dim(imgn)[1]),-5), n_pinpricks, replace=FALSE)
  y_start <- sample(head(seq_len(dim(imgn)[2]),-5), n_pinpricks, replace=FALSE)
  
  # ROIs einzeichnen
  for(i in 1:n_pinpricks){
    
    imgn2[x_start[i]:(x_start[i]+5),y_start[i]:(y_start[i]+5),col_pinprick] <- 1
    
  }
  
  # # Show image
  # display(imgn2)
  
  # Save image
  writeImage(imgn2,files=paste0(folder_results,name_of_analysis,"/sample_pixels_sroi_automation_n",
                                n_pinpricks,"_seed",seed,".jpg"))
  
  
  #################################################################
  #	2. Compute mean %greenness time series in pinpricks
  #		- Save as jpg + RData (optional)
  #		
  # and
  #
  #	3. Compute correlation of all pixel's %greenness time series with the 
  #       pinpricks %greenness time series
  #		- Save correlation images (optional)
  #################################################################
  
  cat(as.character(Sys.time()),": Start compute mean ts in pinpricks and produce correlation images \n")
  
  filename <- paste0(folder_results,
                     name_of_analysis,"/pinpricks_",color,".RData")
  filename_cor <- paste0(folder_results,
                         name_of_analysis,"/corr_images_",color,".RData")
  
  which_images_cor <- which_images
  
  if(color=="green"){
    col_in <- 2
  }else if(color=="red"){
    col_in <- 1
  }else if(color=="blue"){
    col_in <- 3
  }
  
  
  if(get_timeseries){
    
    cat(as.character(Sys.time()),": Start get_timeseries \n")
    
    # List with x- und y-Koordinaten of pinpricks
    
    x_list <- vector(mode="list",length=n_pinpricks)
    y_list <- vector(mode="list",length=n_pinpricks)
    
    for(i in seq_len(n_pinpricks)){
      x_list[[i]] <- x_start[i]:(x_start[i]+5)+1284
      y_list[[i]] <- y_start[i]:(y_start[i]+5)
    }
    
    images_list <- vector("list",n_pinpricks)
    main <- paste0("Pinprick",seq_len(n_pinpricks))
    
    cat(as.character(Sys.time()),": Start read images \n")
    
    # Read all pixels
    images_full <- read_images(colormode="Grayscale",
                               folder=folder_data,
                               file_names=lists_files,
                               which_images=which_images,
                               x=x,
                               y=y,
                               col_out=1,
                               col_in=col_in,
                               sum1=FALSE,
                               norm_it=FALSE,
                               colorspace="rgb", 
                               relative=TRUE)
    
    
    # Cut out pinpricks pixels
    for(i in seq_len(n_pinpricks)){
      
      images_list[[i]] <- images_full[x_list[[i]]-1284,y_list[[i]],,,drop=FALSE]
      
    }
    
    # Deallocate memory
    rm(list="images_full")
    gc()
    
    cat(as.character(Sys.time()),": End read images \n")
    
    tmax <- length(which_images)
    
    # Save pinpricks time series
    save(images_list, doy_c, main, x_list, y_list, which_images,n_pinpricks,tmax,
         file=filename)
    
    cat(as.character(Sys.time()),": get_timeseries done \n")
  }
  
  
  # Compute %greenness time series in pinpricks and produce correlation images
  if(correlate_timeseries){
    
    cat(as.character(Sys.time()),": Start correlate_timeseries \n")
    
    cat(as.character(Sys.time()),": read images \n")
    
    
    images <- read_images(colormode="Grayscale",
                          folder=folder_data,
                          file_names=lists_files,
                          which_images=which_images_cor,
                          x=x,
                          y=y,
                          col_out=1,
                          col_in=col_in,
                          sum1=FALSE,
                          norm_it=FALSE,
                          colorspace="rgb",
                          relative=TRUE)
    
    
    cat(as.character(Sys.time()),": images read in \n")
    
    # Load results of get_timeseries   
    load(filename)
    
    # Array for correlation images
    corr_array <- array(dim=c(length(x), length(y),n_pinpricks))
    
    # Matrix for mean %greenness time series
    timeseries_mean <- matrix(nrow=length(which_images), ncol=n_pinpricks)
    
    # Compute correlation with pinpricks
    for(pinprick in seq_len(n_pinpricks)){
      
      cat(as.character(Sys.time()),": Pinprick", pinprick, "start \n")
      
      # Mean per time point
      timeseries_mean[,pinprick] <- apply(X = images_list[[pinprick]][,,,],
                                        MARGIN = 3,mean, na.rm=TRUE)
      
      # Correlation with pinprick
      corr_array[,,pinprick] <- apply(images[,,,],1:2,FUN=cor, 
                                    y=timeseries_mean[,pinprick],
                                    use="complete.obs")
    }
    
    save(corr_array, timeseries_mean, which_images_cor, which_images,
         file=filename_cor)
    
    cat(as.character(Sys.time()),": correlate_timeseries done \n")
  }
  
  # Plot of results: Correlation images and %greenness time series
  if(do_evaluation){
    
    cat(as.character(Sys.time()),": Start correlation images and greenness time series \n")
    
    if(!get_timeseries){	
      load(filename)
    }
    
    if(!correlate_timeseries){
      load(filename_cor)	
    }
    
    # Save correlation images as jpgs
    for(pinprick in seq_len(n_pinpricks)){
      filename <- paste0(folder_results,name_of_analysis,"/corr_images/",color,"_",
                         main[pinprick],".jpg")
      writeImage(normalize(Image(corr_array[,,pinprick])),filename)
    }
    
    ##############################################
    # %greenness time series in pinpricks
    ##############################################
    
    ylab <-  paste0("rel_",color)
    ylim <- c(.3, .55)
    
    #   pdf(paste0(folder_results,name_of_analysis,"/pinpricks_",color,"_mean.pdf"),
    #       width=12, height=8)
    #   if(n_pinpricks==1){
    #     par(mfrow=c(1,1))
    #   }else{
    #     par(mfrow=c(2,2)) # c(2,3))
    #   }
    #   for(pinprick in seq_len(n_pinpricks)){
    #     plot(timeseries_mean[seq_len(tmax),pinprick], type="l", ylim=ylim,
    #          main=main[pinprick],xlab="Index", ylab=ylab)
    #   }
    #   dev.off()
    
    ###################
    # Mean per DOY
    ###################
    
    mean_doy <- matrix(nrow=length(unique(files_date$doy)), ncol=n_pinpricks)
    
    # Mean per DOY
    for(pinprick in seq_len(n_pinpricks)){
      mean_doy[,pinprick] <- tapply(timeseries_mean[seq_len(tmax),pinprick],
                                  files_date$doy, mean)
    }
    
    # Plot time series averaged per DOY
    pdf(paste0(folder_results,name_of_analysis,"/pinpricks_",color,"_mean_doy.pdf"),
        width=12, height=8)
    if(n_pinpricks==1){
      par(mfrow=c(1,1))
    }else{
      par(mfrow=c(2,2)) # c(2,3))
    }
    for(pinprick in seq_len(n_pinpricks)){
      plot(unique(files_date$doy),mean_doy[,pinprick], type="l", ylim=ylim,
           main=main[pinprick], xlab="DOY", ylab=ylab)	
    }
    dev.off()
    
    cat(as.character(Sys.time()),": End correlation images and greenness time series \n")
  }
  
  
  #################################################################
  #	4. Threshold correlation images to produce ROIs
  #		- Save ROIs as jpg + RData (optional)
  #################################################################
  
  # For each pinpricks and each threshold: Produce ROI
  for(i in seq_len(n_pinpricks)){
    
    pinprick <- i
    
    for(threshold in threshold_vec){
      
      # Produce ROI
      mask <- matrix(0,nrow=1276, ncol=960)
      mask[which(corr_array[,,pinprick]>threshold)] <- 1
      
      # 	# Show ROI
      # 	display(Image(mask))
      
      # Save ROI
      filename <- paste0(folder_results,name_of_analysis,"/masks/pinprick",pinprick,"_threshold",threshold*100)
      save(mask, file=paste0(filename,".RData"))
      writeImage(x = Image(mask), files = paste0(filename, ".jpg"))
    }
  }
  
  cat(as.character(Sys.time()),": ROIs produced \n")
  
  
  #################################################################
  #	5. Compute mean %greenness time series in ROIs
  #		- Average on DOY level
  #################################################################
  
  cat(as.character(Sys.time()),": Start mask images \n")
  
  if(color=="green"){
    col_in <- 2
  }else if(color=="red"){
    col_in <- 1
  }else if(color=="blue"){
    col_in <- 3
  }
  
  cat(as.character(Sys.time()),": read images \n")
  
  x <- 1285:2560
  y <- 1:960
  
  # Read images
  images <- read_images(colormode="Grayscale",
                        folder=folder_data,
                        file_names=lists_files,
                        which_images=which_images,
                        x=x,
                        y=y,
                        col_out=1,
                        col_in=col_in,
                        sum1=FALSE,
                        norm_it=FALSE,
                        colorspace="rgb",
                        relative=TRUE)
  
  
  cat(as.character(Sys.time()),": images read in \n")
  
  gc()
  
  # Matrix for averaged time series
  mean_doy_norm <- matrix(0, 
                          nrow=length(threshold_vec)*n_pinpricks, 
                          ncol= length(unique(doy_c[which_images])))
  
  j <- 0
  
  # For all pinpricks and thresholds
  for(pinprick in seq_len(n_pinpricks)){
    for(i in seq_len(length(threshold_vec))){
      
      j <- j+1
      
      threshold <- threshold_vec[i]
      
      mask_name <- paste0(name_of_analysis,"/pinprick",pinprick,"_threshold",threshold*100)
      mask_name_short <- paste0("pinprick",pinprick,"_threshold",threshold*100)
      
      filename_save <- paste0(folder_results,name_of_analysis,"/mask_timeseries/",
                              mask_name_short,"_",color,".RData")
      
      filename_mask <- paste0(folder_results,name_of_analysis,"/masks/",mask_name_short,".RData")
      
      # Mask images
      load(filename_mask)
      images_masked <- mask_img(images,mask=mask)
      
      cat(as.character(Sys.time()),": images masked \n")
      
      # Compute mean time series
      timeseries_mean <- apply(X = images_masked[,,,],MARGIN = 3,mean, na.rm=TRUE)
      
      # save mean time series
      save(timeseries_mean, 
           file=filename_save)
      
      
      # Mean per DOY mitteln & norm with size of the ROI
      if(mean(mask)==0){
        mean_doy <- tapply(timeseries_mean, doy_c[which_images], mean)
      }else{
        mean_doy <- tapply(timeseries_mean, doy_c[which_images], mean)/mean(mask)
      }	
      
      ylim <- NULL
      ylab <- paste0(color," relative")
      
      pdf(paste0(folder_results,name_of_analysis,"/mask_timeseries/",mask_name_short,"_",color,".pdf"), 
          width=12, height=8)
      par(mfrow=c(1,1))
      plot(unique(doy_c[which_images]), mean_doy, type="l", 
           main=main_camera,
           xlab="DOY", 
           ylab=ylab, 
           ylim=ylim,  
           axes=FALSE,
           col=rgb(0,0,0,.5))
      axis(side = 1, at = unique(doy_c[which_images]),
           labels = tapply(doy[which_images],INDEX = doy_c[which_images], FUN = mean))
      axis(side = 2)
      lines(lowess(unique(doy_c[which_images]),mean_doy,f = .03),
            col="blue", lwd=2, lty=2)
      dev.off()
      
      
      # Norm to (0,1) and average per doy
      time_norm <- timeseries_mean/mean(mask)
      mean_doy_norm[j,] <- tapply(time_norm, doy_c[which_images], mean)
      
      gc()
      
      cat(as.character(Sys.time()),": Pinprick =", pinprick, ", Thresholdmaske =", i, "done \n")
    }
  }
  
  settings_mat <- expand.grid((threshold_vec), seq_len(n_pinpricks))
  colnames(settings_mat) <- c("threshold","pinprick")
  
  save(mean_doy_norm, threshold_vec, settings_mat, x_list, y_list, 
       file=paste0(folder_results,name_of_analysis,"/mask_timeseries/mean_doys_sROI_loop.RData"))
  
  cat(as.character(Sys.time()),": Images masked and mean ts computed \n")
  
  #################################################################
  #	6. Compute optimality criteria for all pinpricks and threshold
  #
  # and 
  #
  # 7. Evaluation
  #		- Which combination of pinprick and threshold is optimal 
  #       with respect to OC1/OC2?
  #		- Plot time series of optimal sROI
  #		- Save sROI
  #################################################################
  
  if(do_strucchange){
    
    # load(paste0(folder_results,"mask_timeseries/sroi/",name_of_analysis,"/","mean_doys_sROI_schleife.RData"))
    
    # With optimality criterion 1
    ftests <- struc_ftest(mean_doy_norm, uROI=TRUE)
    
    # With optimality criterion 2
    max_corr <- struc_dummy(mean_doy_norm, a_vec, b_vec, doy=NULL)
    
    # check mean_greenness$settings_mat to know which mask corresponds to which setting
    settings_mat[ftests$wm1,]
    settings_mat[ftests$wmx,]
    settings_mat[max_corr$wm1,]
    # settings_mat	
    
    settings_oc1_oc2 <- data.frame(settings_mat, ftests$max_f, oc2=max_corr$max_corr)
    
    save(settings_oc1_oc2, file = paste0(folder_results, name_of_analysis,
                                         "/settings_oc1_oc2.RData"))
    # Read background image
    img_color <- Image(read_images(colormode="Color",
                                   folder=folder_data,
                                   file_names=lists_files,
                                   which_images=which_images_background,
                                   x=x,
                                   y=y,
                                   col_out=1:3,
                                   col_in=1:3,
                                   sum1=FALSE,
                                   norm_it=FALSE,
                                   colorspace="rgb"), colormode="Color")
    # Save background image
    writeImage(img_color,files=paste0(folder_results,name_of_analysis,"/background_image.jpg"))
    
    # Find optimal setting
    threshold_opt <-	settings_mat[max_corr$wm1,1] 
    pinprick_opt <-	settings_mat[max_corr$wm1,2]
    
    # Save final sROI
    load(paste0(folder_results,name_of_analysis,"/masks/pinprick",pinprick_opt,"_threshold",
                threshold_opt*100,".RData"))
    imgn2 <- img_color/1.5
    imgn2[,,2,][which(mask==1)] <- 1
    imgn2[,,1,][which(mask==1)] <- 1
    imgn2[,,3,][which(mask==1)] <- 0
    writeImage(imgn2,files=paste0(folder_results,name_of_analysis,
                                  "/best_sroi.jpg"))
    
    # Save image with optimal pinprick
    imgn2 <- img_color/1.5
    imgn2[x_start[max_corr$wm1]:(x_start[max_corr$wm1]+5),y_start[max_corr$wm1]:(y_start[max_corr$wm1]+5),2,] <- 1
    imgn2[x_start[max_corr$wm1]:(x_start[max_corr$wm1]+5),y_start[max_corr$wm1]:(y_start[max_corr$wm1]+5),1,] <- 1
    imgn2[x_start[max_corr$wm1]:(x_start[max_corr$wm1]+5),y_start[max_corr$wm1]:(y_start[max_corr$wm1]+5),3,] <- 0
    writeImage(imgn2,files=paste0(folder_results,name_of_analysis,
                                  "/best_pinprick.jpg"))
    
    cat(as.character(Sys.time()),": End analysis \n")
  }
  
}