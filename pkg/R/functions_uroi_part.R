###########################################################################
# Functions for uROI method
# Date: 18.01.16
# Author: Ludwig Bothmann
###########################################################################

#' Create directories for the results
#'
#' This function creates the specified directories
#'
#' @param path_base Base directory
#' @param name_of_analysis Name of the analysis
#' @return Directories \code{path_base/name_of_analysis/mask_timeseries} and
#'  \code{path_base/name_of_analysis/masks} are created
#' @export
#'
create_dirs_uroi <- function(path_base=NULL,
                             name_of_analysis)
{
  
  if(!dir.exists(path_base)){
    dir.create(path_base, recursive=TRUE)
  }
  
  if(!dir.exists(paste0(path_base,name_of_analysis))){
    dir.create(paste0(path_base,name_of_analysis))
  }
  if(!dir.exists(paste0(path_base,name_of_analysis,"/mask_timeseries"))){
    dir.create(paste0(path_base,name_of_analysis,"/mask_timeseries"))
  }
  if(!dir.exists(paste0(path_base,name_of_analysis,"/masks"))){
    dir.create(paste0(path_base,name_of_analysis,"/masks"))
  }
  
  # 	if(!dir.exists(paste0(path_base,"mask_timeseries"))){
  # 		dir.create(paste0(path_base,"mask_timeseries"))
  # 	}
  # 	if(!dir.exists(paste0(path_base,"mask_timeseries/uroi/"))){
  # 		dir.create(paste0(path_base,"mask_timeseries/uroi/"))
  # 	}
  # 	if(!dir.exists(paste0(path_base,"mask_timeseries/uroi/",date_analysis,"/"))){
  # 		dir.create(paste0(path_base,"mask_timeseries/uroi/",date_analysis,"/"))
  # 	}
  #
  #
  # 	if(!dir.exists(paste0(path_base,"masks"))){
  # 		dir.create(paste0(path_base,"masks"))
  # 	}
  # 	if(!dir.exists(paste0(path_base,"masks/uroi/"))){
  # 		dir.create(paste0(path_base,"masks/uroi/"))
  # 	}
  # 	if(!dir.exists(paste0(path_base,"masks/uroi/",date_analysis,"/"))){
  # 		dir.create(paste0(path_base,"masks/uroi/",date_analysis,"/"))
  # 	}
  
  
}

#' Generate binary masks from the results of a cluster analysis
#' 
#' This function transforms the results of a cluster analysis in a set of 
#'  binary masks (or ROIs).
#'
#' @param settings A matrix containing the analysed settings.
#' 	Preferably the output from \code{\link{find_clusters}}.
#' @param cluster_list A list with the results of the cluster analysis.
#' 	Preferably the output from \code{\link{find_clusters}}.
#' @param x Vector of x-coordinates to read in. Default \code{NULL} results in 
#'  a grid of all possible values.
#' @param y Vector of y-coordinates to read in. Default \code{NULL} results in 
#'  a grid of all possible values.
#' @param name_of_analysis Name of the analysis (for file name of the results)
#' @param path_base Base directory
#' @param return_masks \code{TRUE} (default): Masks are returned as list.
#' @param save_masks \code{TRUE} (default): Masks are saved as .RData and .tiff.
#' @param type Type of saved images: .tiff, .jpeg, .jpg, .png, ..., everything
#' 	possible in \code{EBImage::writeImage}, see \code{?EBImage::writeImage}
#' @return List of masks.
#' @importFrom EBImage writeImage
#' @importFrom EBImage Image
#' @importFrom EBImage imageData
#' @export
generate_masks <- function(settings,
                           cluster_list,
                           x=NULL,
                           y=NULL,
                           # folder,
                           # lists_files,
                           name_of_analysis,
                           path_base,
                           return_masks=TRUE,
                           # save_masks_rdata=TRUE,
                           # save_masks_jpg=TRUE,
                           save_masks=TRUE,
                           type=".tiff"
){
  
  # If no dimensions are specified or just one dimension is specified: read the remaining dimensions
  # from the image itself
  if(is.null(x) | is.null(y)){
    
    # 		img_dim <- readImage(paste0(folder,lists_files[1]))
    # 		if(is.null(x))	x <- seq_len(dim(img_dim)[1])
    # 		if(is.null(y)) y <- seq_len(dim(img_dim)[2])
    #
    # 		rm(list="img_dim")
    # 		gc()
    if(is.null(x)) x <- seq_len(nrow(cluster_list[[1]]))
    if(is.null(y)) y <- seq_len(ncol(cluster_list[[1]]))
  }
  
  if(return_masks){
    masks_list <- vector(mode = "list", length = sum(settings$K))
  }
  
  m <- 0
  
  # Generate binary masks for each setting ...
  for(i in seq_len(nrow(settings))){
    
    clu <- cluster_list[[i]]
    
    cluI <- Image(clu,
                  dim=c(length(x), length(y)),
                  colormode="Grayscale")
    
    # ... and for each cluster
    for(k in seq_len(settings[i,1])){
      
      # Generate 0/1 mask
      mask <- (imageData(cluI)==k)*1
      
      if(save_masks){
        # Save mask
        filename <- paste0(path_base,name_of_analysis,"/masks/npc",
                           settings[i,2],"_K",settings[i,1],"_",k)
        
        save(mask, file=paste0(filename,".RData"))
        writeImage(x = Image(mask), files = paste0(filename, type))
      }
      
      m <- m+1
      
      if(return_masks) masks_list[[m]] <- mask
    }
  }
  
  if(return_masks){
    return(masks_list)
  }else{
    return(NULL)
  }
}

#' Mask images and compute \%greenness time series
#' 
#' For given ROIs, the \%greenness time series inside the ROIs is computed on doy level
#'
#' @param color Color to compute the \%color time series. Default is \code{"green"}
#' @param x Vector of x-coordinates to read in. Default \code{NULL} results in 
#'  a grid of all possible values.
#' @param y Vector of y-coordinates to read in. Default \code{NULL} results in 
#'  a grid of all possible values.
#' @param folder The folder which contains the images.
#' @param lists_files A vector containing the names of all images inside 
#'  \code{folder} to be read in.
#' @param which_images An optional vector to select images from \code{lists_files}.
#' 	Default is \code{seq_len(length(lists_files))}
#' @param doy Vector of DOYs for each image
#' @param path_base Base directory for saving the results
#' @param name_of_analysis Name of the analysis (for file name of the results)
#' @param main_plot Main for the plot
#' @param settings_mat Matrix with all settings from find_clusters
#' @param load_masks Boolean: Shall the masks be loaded from the disc? Otherwise, 
#'  results from \code{\link{generate_masks}} can be given as \code{masks_list}
#' @param masks_list A list containing the masks, possibly output from \code{\link{generate_masks}}
#' @return A list containing the mean doy time series and settings
#' @import grDevices
#' @import graphics
#' @import stats
#' @export
compute_greenness_time_series  <- function(color="green",
                                           x=NULL,
                                           y=NULL,
                                           folder,
                                           lists_files,
                                           which_images=seq_len(length(lists_files)),
                                           # 														 n_pc_vec,
                                           # 														 k_vec,
                                           doy,
                                           path_base,
                                           name_of_analysis,
                                           main_plot,
                                           settings_mat,
                                           load_masks=FALSE,
                                           masks_list=NULL)
{
  
  if(!load_masks & is.null(masks_list)) stop("Either set load_masks=TRUE or specify masks_list.")
  
  cat(as.character(Sys.time()),": start masking\n")
  
  if(color=="green"){
    col_in <- 2
  }else if(color=="red"){
    col_in <- 1
  }else if(color=="blue"){
    col_in <- 3
  }
  
  # If no dimensions are specified or just one dimension is specified: read the remaining dimensions
  # from the image itself
  if(is.null(x) | is.null(y)){
    
    # 		img_dim <- readImage(paste0(folder,lists_files[1]))
    # 		if(is.null(x))	x <- seq_len(dim(img_dim)[1])
    # 		if(is.null(y)) y <- seq_len(dim(img_dim)[2])
    # 
    # 		rm(list="img_dim")
    # 		gc()
    
    if(is.null(x)) x <- seq_len(nrow(masks_list[[1]]))
    if(is.null(y)) y <- seq_len(ncol(masks_list[[1]]))
    
  }
  
  cat(as.character(Sys.time()),": read images \n")
  
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
                        relative=TRUE,
                        main_plot)
  
  # 	# Generate a matrix which contains for each setting one row with informations:
  # 	#   - how many singular vectors?
  # 	#   - how many clusters for k-means?
  # 	#   - which of these clusters is considered at the moment?
  #
  # 	k_long <- c()
  #
  # 	for(k in k_vec){
  #
  # 		k_long <- c(k_long, seq_len(k))
  # 	}
  #
  # 	settings_mat <- cbind(rep(k_vec, k_vec), k_long, rep(n_pc_vec, each=sum(k_vec)))
  # 	colnames(settings_mat) <- c("K", "k", "n_pc")
  
  n_settings <- nrow(settings_mat)
  
  # Hier kommen die pro Tag gemittelten Kurven rein
  mean_doy_norm <- matrix(0,
                          nrow = n_settings,
                          ncol = length(unique(doy[which_images])))
  
  # Alle Masken aller Settings durchgehen
  for(i in seq_len(n_settings)){
    
    n_pc <- settings_mat[i,3]
    K <- settings_mat[i,1]
    k <- settings_mat[i,2]
    # cat(as.character(Sys.time()),": Start npc =", n_pc, ", K =", K,", k =", k, "\n")
    
    mask_name <- paste0("npc",n_pc,"_K",K,"_",k)
    
    filename_save <- paste0(path_base,name_of_analysis,"/mask_timeseries/",
                            mask_name,"_",color,".RData")
    # Mask images
    if(load_masks){
      filename_mask <- paste0(path_base,name_of_analysis,"/masks/",mask_name,".RData")
      load(filename_mask)
    }else{
      mask <- masks_list[[i]]
    }
    
    images_masked <- mask_img(images,mask=mask)
    
    # cat(as.character(Sys.time()),": images masked \n")
    
    # Compute mean cluster time series ...
    timeseries_mean <- apply(X = images_masked[,,,],MARGIN = 3,mean, na.rm=TRUE)
    
    # ... and save it
    save(timeseries_mean,
         file=filename_save)
    
    # Mean per DOY & norm with mask size
    if(mean(mask)==0){
      mean_doy <- tapply(timeseries_mean, doy[which_images], mean)
    }else{
      mean_doy <- tapply(timeseries_mean, doy[which_images], mean)/mean(mask)
    }
    
    # plot
    ylim <- NULL
    ylab <- paste0(color," relative")
    
    jpeg(paste0(path_base,name_of_analysis,"/mask_timeseries/",mask_name,"_",color,
                ".jpg"), #_mean_doy
         width=12, height=8, units="in", res=100)
    # 		pdf(paste0(path_base,name_of_analysis,"/mask_timeseries/",mask_name,"_",color,
    # 					  ".pdf"), #_mean_doy
    # 			 width=12, height=8)
    par(mfrow=c(1,1))
    plot(unique(doy[which_images]), mean_doy, type="l",
         main=main_plot,
         xlab="DOY",
         ylab=ylab,
         ylim=ylim,
         axes=FALSE,
         col=rgb(0,0,0,.5))
    
    #########################
    # wieder auskommentieren?
    axis(side = 1, at = unique(doy[which_images]),
         labels = tapply(doy[which_images],INDEX = doy[which_images], FUN = mean))
    axis(side = 2)
    #########################
    lines(lowess(unique(doy[which_images]),mean_doy,f = .03),
          col="blue", lwd=2, lty=2)
    
    dev.off()
    
    # Norm values to (0,1) and average per DOY
    time_norm <- timeseries_mean/mean(mask)
    mean_doy_norm[i,] <- tapply(time_norm, doy[which_images], mean)
    colnames(mean_doy_norm) <- unique(doy[which_images])
    
    gc()
    
    cat(as.character(Sys.time()),": npc =", n_pc, ", K =", K, ", k =", k, "done \n")
    # cat("============================================ \n \n")
    
  }
  
  save(mean_doy_norm, settings_mat,
       file=paste0(path_base,name_of_analysis,"/mask_timeseries/mean_doys_uROI_",color,".RData"))
  
  cat(as.character(Sys.time()),": masking done \n")
  # cat("============================================ \n \n")
  
  output <- list(mean_doy_norm=mean_doy_norm,
                 settings_mat=settings_mat)
  return(output)
}

#' Overlay list of mask images on reference image with transparency alpha
#'
#' @param masks_list List of masks, preferably output from \code{\link{generate_masks}}
#' @param ref_image Reference image for the overlay
#' @param alpha Transparency factor
#' @return An \code{Image} object containing all overlays.
#' @author Michael Matiu
#' @import abind
#' @importFrom EBImage Image
#' @importFrom EBImage imageData
#' @importFrom EBImage colorMode
#' @export
overlay_masks_onref <- function(masks_list, ref_image, alpha = 0.1){
  image_masks <- lapply(masks_list, function(mask){
    if(colorMode(ref_image) == 0){
      mask_numeric <- as.numeric(mask)
    } else {
      mask_numeric <- as.numeric(abind::abind(mask,mask,mask, rev.along = 0))
    }
    mask_numeric[mask_numeric == 0] <- alpha
    Image(imageData(ref_image) * mask_numeric,
          dim = dim(ref_image),
          colormode = colorMode(ref_image))
  })
  do.call(combine, image_masks)
}
