##############################################
# Automated sROI-approach
# 
# Date: 01.07.16
# Autoren: Ludwig Bothmann
##############################################

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

#################################################################
#	0. Set parameters
#################################################################

rm(list=ls())

setwd("/home/bothmannlu/Desktop/sroi_test/")

library("EBImage")
library("irlba")
library("strucchange")

# # Load package "phenofun"
# library(devtools)
# load_all("../rforge/phenofun/pkg")

# Install package phenofun from R-Forge
install.packages("phenofun", repos="http://R-Forge.R-project.org")

# Load package "phenofun"
library(phenofun)
# help(package="phenofun")

# Which parts of the analysis shall be carried out?
get_timeseries <- TRUE
correlate_timeseries <- TRUE
do_evaluation <- TRUE
do_strucchange <- TRUE

# Color channel of interest
color <- "green"

# Name of analyis (for path of results)
name_of_analysis <- "151110_2014"

# Folder of results
path_base <- "results/"

# Folder of data
folder <- "../../Dokumente/Phenology/data/WebCam/cam7/"

# Vector with file names of relevant images
lists_files <- list.files(path=folder)[-(1:8)]

# Number of images to be analyzed
which_images <- c(1001:2441)[round(seq(1,1441,length=100))]

# Which image is background image for overlay plots?
which_images_background <- 1500

# Extract DOYs
doy <- as.numeric(substr(lists_files,26,28))

# DOY continous over several years
doy_c <- doy
doy_c[1001:2441] <- doy_c[1001:2441] + 365
doy_c[2442:2466] <- doy_c[2442:2466] + 2*365

# Title for plot
main_camera <- "Kranzberg 2 - 2014"

files_date <- data.frame(doy=doy, doy_c=doy_c)[which_images,]

# Number of random pinpricks
n_regions <- 2

# Set seed
seed <- 1014
set.seed(seed)

# Vector with thresholds for correlations
threshold_vec <- seq(.5, .9, length=2)#by=.05)

# Create directories
dir.create(paste0(path_base,name_of_analysis,"/corr_images/"), recursive = TRUE)
dir.create(paste0(path_base,name_of_analysis,"/mask_timeseries/"), recursive = TRUE)
dir.create(paste0(path_base,name_of_analysis,"/masks/"), recursive = TRUE)


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
                      folder=folder,
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

x_start <- sample(head(seq_len(dim(imgn)[1]),-5), n_regions, replace=FALSE)
y_start <- sample(head(seq_len(dim(imgn)[2]),-5), n_regions, replace=FALSE)

# ROIs einzeichnen
for(i in 1:n_regions){
  
  imgn2[x_start[i]:(x_start[i]+5),y_start[i]:(y_start[i]+5),col_pinprick] <- 1
  
}

# # Show image
# display(imgn2)

# Save image
writeImage(imgn2,files=paste0(path_base,name_of_analysis,"/sample_pixels_sroi_automation_n",
                              n_regions,"_seed",seed,".jpg"))


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

filename <- paste0(path_base,
                   name_of_analysis,"/regions_",color,".RData")
filename_cor <- paste0(path_base,
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
  
  x_list <- vector(mode="list",length=n_regions)
  y_list <- vector(mode="list",length=n_regions)
  
  for(i in seq_len(n_regions)){
    x_list[[i]] <- x_start[i]:(x_start[i]+5)+1284
    y_list[[i]] <- y_start[i]:(y_start[i]+5)
  }
  
  images_list <- vector("list",n_regions)
  main <- paste0("Pinprick",seq_len(n_regions))
  
  cat(as.character(Sys.time()),": Start read images \n")
  
  # Read all pixels
  images_full <- read_images(colormode="Grayscale",
                             folder=folder,
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
  for(i in seq_len(n_regions)){
    
    images_list[[i]] <- images_full[x_list[[i]]-1284,y_list[[i]],,,drop=FALSE]
    
  }
  
  # Deallocate memory
  rm(list="images_full")
  gc()
  
  cat(as.character(Sys.time()),": End read images \n")
  
  tmax <- length(which_images)
  
  # Save pinpricks time series
  save(images_list, doy_c, main, x_list, y_list, which_images,n_regions,tmax,
       file=filename)
  
  cat(as.character(Sys.time()),": get_timeseries done \n")
}


# Compute %greenness time series in pinpricks and produce correlation images
if(correlate_timeseries){
  
  cat(as.character(Sys.time()),": Start correlate_timeseries \n")
  
  cat(as.character(Sys.time()),": read images \n")
  
  
  images <- read_images(colormode="Grayscale",
                        folder=folder,
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
  corr_array <- array(dim=c(length(x), length(y),n_regions))
  
  # Matrix for mean %greenness time series
  timeseries_mean <- matrix(nrow=length(which_images), ncol=n_regions)
  
  # Compute correlation with pinpricks
  for(region in seq_len(n_regions)){
    
    cat(as.character(Sys.time()),": Pinprick", region, "start \n")
    
    # Mean per time point
    timeseries_mean[,region] <- apply(X = images_list[[region]][,,,],
                                      MARGIN = 3,mean, na.rm=TRUE)
    
    # Correlation with pinprick
    corr_array[,,region] <- apply(images[,,,],1:2,FUN=cor, 
                                  y=timeseries_mean[,region],
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
  for(region in seq_len(n_regions)){
    filename <- paste0(path_base,name_of_analysis,"/corr_images/",color,"_",
                       main[region],".jpg")
    writeImage(normalize(Image(corr_array[,,region])),filename)
  }
  
  ##############################################
  # %greenness time series in pinpricks
  ##############################################
  
  ylab <-  paste0("rel_",color)
  ylim <- c(.3, .55)
  
  #   pdf(paste0(path_base,name_of_analysis,"/pinpricks_",color,"_mean.pdf"),
  #       width=12, height=8)
  #   if(n_regions==1){
  #     par(mfrow=c(1,1))
  #   }else{
  #     par(mfrow=c(2,2)) # c(2,3))
  #   }
  #   for(region in seq_len(n_regions)){
  #     plot(timeseries_mean[seq_len(tmax),region], type="l", ylim=ylim,
  #          main=main[region],xlab="Index", ylab=ylab)
  #   }
  #   dev.off()
  
  ###################
  # Mean per DOY
  ###################
  
  mean_doy <- matrix(nrow=length(unique(files_date$doy)), ncol=n_regions)
  
  # Mean per DOY
  for(region in seq_len(n_regions)){
    mean_doy[,region] <- tapply(timeseries_mean[seq_len(tmax),region],
                                files_date$doy, mean)
  }
  
  # Plot time series averaged per DOY
  pdf(paste0(path_base,name_of_analysis,"/pinpricks_",color,"_mean_doy.pdf"),
      width=12, height=8)
  if(n_regions==1){
    par(mfrow=c(1,1))
  }else{
    par(mfrow=c(2,2)) # c(2,3))
  }
  for(region in seq_len(n_regions)){
    plot(unique(files_date$doy),mean_doy[,region], type="l", ylim=ylim,
         main=main[region], xlab="DOY", ylab=ylab)	
  }
  dev.off()
  
  cat(as.character(Sys.time()),": End correlation images and greenness time series \n")
}


#################################################################
#	4. Threshold correlation images to produce ROIs
#		- Save ROIs as jpg + RData (optional)
#################################################################

# For each pinpricks and each threshold: Produce ROI
for(i in seq_len(n_regions)){
  
  region <- i
  
  for(threshold in threshold_vec){
    
    # Produce ROI
    mask <- matrix(0,nrow=1276, ncol=960)
    mask[which(corr_array[,,region]>threshold)] <- 1
    
    # 	# Show ROI
    # 	display(Image(mask))
    
    # Save ROI
    filename <- paste0(path_base,name_of_analysis,"/masks/pinprick",region,"_threshold",threshold*100)
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
                      folder=folder,
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
                        nrow=length(threshold_vec)*n_regions, 
                        ncol= length(unique(doy_c[which_images])))

j <- 0

# For all pinpricks and thresholds
for(region in seq_len(n_regions)){
  for(i in seq_len(length(threshold_vec))){
    
    j <- j+1
    
    threshold <- threshold_vec[i]
    
    mask_name <- paste0(name_of_analysis,"/pinprick",region,"_threshold",threshold*100)
    mask_name_short <- paste0("pinprick",region,"_threshold",threshold*100)
    
    filename_save <- paste0(path_base,name_of_analysis,"/mask_timeseries/",
                            mask_name_short,"_",color,".RData")
    
    filename_mask <- paste0(path_base,name_of_analysis,"/masks/",mask_name_short,".RData")
    
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
    
    pdf(paste0(path_base,name_of_analysis,"/mask_timeseries/",mask_name_short,"_",color,".pdf"), 
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
    
    cat(as.character(Sys.time()),": Pinprick =", region, ", Thresholdmaske =", i, "done \n")
  }
}

settings_mat <- expand.grid((threshold_vec), seq_len(n_regions))
colnames(settings_mat) <- c("threshold","pinprick")

save(mean_doy_norm, threshold_vec, settings_mat, x_list, y_list, 
     file=paste0(path_base,name_of_analysis,"/mask_timeseries/mean_doys_sROI_loop.RData"))

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
  
  # load(paste0(path_base,"mask_timeseries/sroi/",name_of_analysis,"/","mean_doys_sROI_schleife.RData"))
  
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
  
  save(settings_oc1_oc2, file = paste0(path_base, name_of_analysis,
                                       "/settings_oc1_oc2.RData"))
  # Read background image
  img_color <- Image(read_images(colormode="Color",
                                 folder=folder,
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
  writeImage(img_color,files=paste0(path_base,name_of_analysis,"/background_image.jpg"))
  
  # Find optimal setting
  threshold_opt <-	settings_mat[max_corr$wm1,1] 
  pinprick_opt <-	settings_mat[max_corr$wm1,2]
  
  # Save final sROI
  load(paste0(path_base,name_of_analysis,"/masks/pinprick",pinprick_opt,"_threshold",
              threshold_opt*100,".RData"))
  imgn2 <- img_color/1.5
  imgn2[,,2,][which(mask==1)] <- 1
  imgn2[,,1,][which(mask==1)] <- 1
  imgn2[,,3,][which(mask==1)] <- 0
  writeImage(imgn2,files=paste0(path_base,name_of_analysis,
                                "/best_sroi.jpg"))
  
  # Save image with optimal pinprick
  imgn2 <- img_color/1.5
  imgn2[x_start[max_corr$wm1]:(x_start[max_corr$wm1]+5),y_start[max_corr$wm1]:(y_start[max_corr$wm1]+5),2,] <- 1
  imgn2[x_start[max_corr$wm1]:(x_start[max_corr$wm1]+5),y_start[max_corr$wm1]:(y_start[max_corr$wm1]+5),1,] <- 1
  imgn2[x_start[max_corr$wm1]:(x_start[max_corr$wm1]+5),y_start[max_corr$wm1]:(y_start[max_corr$wm1]+5),3,] <- 0
  writeImage(imgn2,files=paste0(path_base,name_of_analysis,
                                "/best_pinprick.jpg"))
  
  cat(as.character(Sys.time()),": End analysis \n")
}