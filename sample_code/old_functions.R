# ###################################
# # eigen_images()
# ###################################
# 
# #' Function to compute  the eigenimages of a given set of images
# #' 
# #' Images can be either grayscale images or color images. 
# #' 	The function computes the eigenimages (considering each pixel
# #' 	as different variable, i.e. no penalization) and eigenvalues.
# #' 	
# #' @param images array of pixel intensities, 3rd dimension has to be the
# #' 	color channels, 4th dimension the time. For grayscale images this
# #' 	means that \code{dim(images) = (*,*,1,*)}, for color images 
# #' 	\code{dim(images) = (*,*,3,*)}
# #' @param n_pc number of eigenimages
# #' @param varimax_rotation boolean: Shall varimax rotation be performed?
# #' @import EBImage
# #' @import irlba
# #' @export
# eigen_images <- function(images,
# 								 n_pc=5,
# 								 varimax_rotation=FALSE){
# 	
# 	# Dimension of the images array
# 	dim_images <- dim(images)
# 	
# 	# Flat: Matrix with 
# 	#		#columns = #images
# 	#		#rows = #pixels*color channels
# 	images_flat <- array(images, 
# 								dim=c(prod(head(dim_images, -1)),
# 										tail(dim_images, 1)))
# 	
# 	# Compute mean image and centered image
# 	# 	images_flat_mean <- rowMeans(images_flat)
# 	# 	images_flat_centered <- images_flat - images_flat_mean
# 	images_flat_centered <- images_flat - rowMeans(images_flat)
# 	
# 	cat(as.character(Sys.time()),"Start svd \n")
# 	
# 	# Fast singular value decomposition (package irlba)
# 	#	left singular vector are the eigenimages
# 	svd_images <- irlba(images_flat_centered, nu=n_pc, nv=0,work=n_pc+5)
# 	
# 	cat(as.character(Sys.time()),"svd complete \n")
# 	
# 	# Compute total variance to evaluate in later step, how much variance
# 	#	is explained by the eigenimages
# 	total_variance <- sum(apply(images_flat, MARGIN = 1, var))
# 	
# 	cat(as.character(Sys.time()),"total_variance complete \n")
# 	
# 	if(varimax_rotation){
# 		
# 		# Rotate Eigenimages
# 		svd_images_rot <- varimax(svd_images$u, normalize=FALSE)$loadings
# 		
# 		# Rearrange Eigenimages in array
# 		eigenimages <- array(svd_images_rot, 
# 									dim=c(head(dim_images, -1), 
# 											n_pc))
# 		
# 		# How can I compute the explained variance now?
# 		return(list(eigenimages = eigenimages,
# 						total_variance = total_variance))
# 		
# 	}else{
# 		
# 		# Rearrange Eigenimages in array
# 		eigenimages <- array(svd_images$u, 
# 									dim=c(head(dim_images, -1), 
# 											n_pc))
# 		
# 		return(list(eigenimages = eigenimages,
# 						svd_values = svd_images$d,
# 						total_variance = total_variance))
# 	}
# 	
# }
# 
# # ###################################
# # display_eigen()
# ###################################
# 
# #' Display the eigenimages and effect of a multiple on the mean image
# #' 
# #' This function displays the mean image and all eigenimages. 
# #' 	Additionaly, the sum and difference of the mean image and a 
# #' 	suitable multiple of the images is shown optionally.
# #' 	
# #' @param eigenimages eigen images as 3D or 4D array, depending on the
# #' 			color mode
# #' @param colormode either \code{Color} or \code{Grayscale}
# #' @param n_pc  number of eigenimages
# #' @param images_flat_mean mean image as vector
# #' @param fact the factor, the eigenimages should be multiplied with
# #' @param do_sum_diff \code{TRUE}: Sum and difference are displayed - 
# #' 			default.# At the moment, this option works only for 
# #' 			\code{sum1=FALSE}
# #' @param sum1 \code{TRUE}: Pixel intensities are normed to sum=1, i.e. 
# #' 	relative intensities are computed, default is \code{FALSE}
# #' @param save_it \code{TRUE}: Images are saved rather than displayed, 
# #' 	default is \code{FALSE}
# #' @param save_file filename to save the images
# #' @param show_eigen \code{TRUE}: Eigenimages are displayed/saved,
# #' 	default is \code{TRUE}
# #' @param colorspace either \code{"rgb"} or \code{"hsv"}, 
# #' 	default is \code{"rgb"}
# #' @export
# display_eigen <- function(eigenimages,
# 								  colormode = "Color",
# 								  n_pc,
# 								  images_flat_mean,
# 								  fact=100,
# 								  do_sum_diff=TRUE,
# 								  sum1=FALSE,
# 								  save_it=FALSE,
# 								  save_file=NULL,
# 								  show_eigen=TRUE,
# 								  colorspace="rgb"
# ){
# 	
# 	if(!is.element(colorspace,c("rgb","hsv"))){
# 		stop("colorspace has to be rgb or hsv")
# 	} 
# 	# 	
# 	# 	if(colorspace=="hsv" & show_eigen){
# 	# 		show_eigen <- FALSE
# 	# 		warning("show_eigen still does not work for colorspace hsv => show_eigen set to FALSE")
# 	# 	}
# 	
# 	if(save_it & is.null(save_file)){
# 		stop("File name not provided, don't know where to save the images")	
# 	}
# 	
# 	# 	if(do_sum_diff & sum1){
# 	# 		warning("do_sum_diff + sum1 at the moment not implemented")
# 	# 	}
# 	if(colormode=="Grayscale" & sum1){
# 		stop("It does not make sense to standardize a Grayscale image to sum=1 \n")
# 	}
# 	
# 	if(save_it){
# 		if(!file.exists(save_file)) dir.create(save_file)
# 	}
# 	
# 	dim_images <- dim(eigenimages)
# 	
# 	# Compute mean image
# 	if(colorspace=="rgb"){
# 		
# 		image_mean <- Image(images_flat_mean, 
# 								  dim=head(dim_images, -1), 
# 								  colormode=colormode)
# 	}else{
# 		image_mean_array <- array(images_flat_mean, 
# 										  dim=head(dim_images, -1))
# 		
# 		image_mean <- Image(hsv_to_rgb(image_mean_array),
# 								  dim=head(dim_images, -1),
# 								  colormode=colormode)
# 	}
# 	
# 	if(sum1){
# 		
# 		image_mean <- rgbImage(red=image_mean[,,1],
# 									  green=image_mean[,,2],
# 									  blue=(1-image_mean[,,1] - image_mean[,,2]))
# 		
# 	}
# 	
# 	# Restore third colour channel in eigenimages (blue channel)
# 	if(sum1){
# 		
# 		# Schoen ist das nicht!
# 		# Wahrscheinlich stimmt es noch nicht mal
# 		#	=> Erstmal theoretisch ueberlegen!
# 		
# 		dim_eigen <- dim_images
# 		dim_eigen[3] <- 3
# 		
# 		eigenimages_flat <- array(eigenimages, 
# 										  dim=c(prod(head(dim_images, -1)),
# 										  		n_pc))
# 		
# 		eigenimages_uncentered  <- eigenimages_flat + images_flat_mean
# 		
# 		# Rearrange uncentered Eigenimages in array
# 		eigenimages_array <- array(eigenimages_uncentered, 
# 											dim=c(head(dim_images, -1), 
# 													n_pc))
# 		
# 		# Rearrange dimensions to have a matrix with 2 columns
# 		eigenimages_mat <- array(eigenimages_array,
# 										 dim=c(prod(head(dim_images, 2),
# 										 			  tail(dim_images, 1)),
# 										 		dim_images[3]))
# 		
# 		# Restore blue channel
# 		blue <- rowSums(eigenimages_mat)
# 		
# 		eigenimages_3channels <- cbind(eigenimages_mat,blue)
# 		
# 		# And put everything together
# 		eigen_restored <- aperm(array(eigenimages_3channels,
# 												dim=dim_eigen[c(1,2,4,3)]),
# 										c(1,2,4,3))
# 		
# 	}else{
# 		
# 		# Dimensions for image vary with colormode
# 		if(colormode=="Color"){
# 			dim_eigen <- c(head(dim_images, -1), n_pc)
# 		}else{
# 			dim_eigen <- c(head(dim_images, 2), n_pc)
# 		}
# 	}	
# 	
# 	# Show mean image and eigenimages
# 	if(show_eigen){
# 		
# 		# Convert Eigenimages in Image object, if hsv colorspace: 
# 		#	transform to rgb colorspace
# 		if(colorspace=="rgb"){
# 			eigen_Image <- Image(eigenimages, 
# 										colormode=colormode,
# 										dim=dim_eigen)
# 		}else{
# 			
# 			# This does not work currently because eigenimages has negative
# 			#	values: add minimum?
# 			# I use normalize now - but I am not sure if this is what I want.
# 			
# 			eigen_Image <- Image(hsv_to_rgb(normalize(eigenimages,
# 																	separate=FALSE)), 
# 										colormode=colormode,
# 										dim=dim_eigen)
# 		}
# 		
# 		if(save_it){
# 			filenames <- c(paste0(save_file,"mean.jpg"),paste0(save_file,"eigen",(1:(n_pc)),".jpg"))
# 			writeImage(normalize(EBImage::combine(image_mean,
# 															  eigen_Image)),
# 						  files=filenames)
# 			
# 		}else{
# 			display(normalize(EBImage::combine(image_mean,
# 														  eigen_Image)))
# 		}
# 	}
# 	
# 	if(do_sum_diff){
# 		
# 		# Sum of mean image and eigenimages
# 		eigenimages_flat <- array(eigenimages, 
# 										  dim=c(prod(head(dim_images, -1)),
# 										  		n_pc))
# 		sum_flat <- images_flat_mean + eigenimages_flat*fact
# 		sum_images <- array(sum_flat, dim=c(head(dim_images, -1), n_pc))	
# 		
# 		if(colorspace=="rgb"){
# 			sum_images_Image <- Image(sum_images, 
# 											  colormode=colormode,
# 											  dim=dim_eigen)
# 		}else{
# 			sum_images_Image <- Image(hsv_to_rgb(normalize(sum_images,
# 																		  separate=FALSE)), 
# 											  colormode=colormode,
# 											  dim=dim_eigen)
# 		}
# 		
# 		# Add blue channel
# 		if(sum1){			
# 			sum_images <- rgbImage(red=sum_images[,,1,],
# 										  green=sum_images[,,2,],
# 										  blue=(1-sum_images[,,1,] - sum_images[,,2,]))
# 		}
# 		
# 		if(save_it){
# 			filenames <- c(paste0(save_file,"mean.jpg"),paste0(save_file,"sum",(1:(n_pc)),".jpg"))
# 			writeImage(normalize(EBImage::combine(image_mean,
# 															  sum_images_Image)),
# 						  files=filenames)
# 		}else{
# 			display(normalize(EBImage::combine(image_mean,
# 														  sum_images_Image)))
# 		}
# 		
# 		# Difference of mean image and eigenimages
# 		diff_flat <- images_flat_mean - eigenimages_flat*fact
# 		diff_images <- array(diff_flat, dim=c(head(dim_images, -1), n_pc))	
# 		
# 		if(colorspace=="rgb"){
# 			diff_images_Image <- Image(diff_images, 
# 												colormode=colormode,
# 												dim=dim_eigen)
# 		}else{
# 			diff_images_Image <- Image(hsv_to_rgb(normalize(diff_images,
# 																			separate=FALSE)), 
# 												colormode=colormode,
# 												dim=dim_eigen)
# 		}
# 		
# 		# Add blue channel
# 		if(sum1){			
# 			diff_images <- rgbImage(red=diff_images[,,1,],
# 											green=diff_images[,,2,],
# 											blue=(1-diff_images[,,1,] - diff_images[,,2,]))
# 		}
# 		
# 		if(save_it){
# 			filenames <- c(paste0(save_file,"mean.jpg"),paste0(save_file,"diff",(1:(n_pc)),".jpg"))
# 			writeImage(normalize(EBImage::combine(image_mean,
# 															  diff_images_Image)),
# 						  files=filenames)
# 		}else{
# 			display(normalize(EBImage::combine(image_mean,
# 														  diff_images_Image)))	
# 		}
# 	}
# }
# 
# # ###################################
# # display_scores()
# ###################################
# 
# #' Plot scores on different eigenimages
# #' 
# #' This function averages the scores of all images taken at the same day
# #' 	and plots these averaged scores.
# #' 	
# #' @param scores matrix of scores, row \code{i} contains all scores to the
# #' 			\code{i}-th eigenimage
# #' @param year vector containing the year of the image
# #' @param doy vector containing the doy of the image
# #' @param complete_year \code{TRUE} complete years specified in 
# #' 			which_year is displayed, otherwise the scores of
# #' 			\code{which_images} are displayed, default is \code{TRUE}
# #' @param which_year vector specifying the years to be shown
# #' @param which_images vector specifying the indices of scores to be shown
# #' @param n_show number of shown scores time series
# #' @param save_it \code{TRUE}: Plots are saved rather than displayed, 
# #' 	default is \code{FALSE}
# #' @param save_file filename to save the images
# #' @param main title of the plots
# #' @param sep_plots \code{TRUE}: Each score in a separate plot, 
# #' 	default is \code{FALSE}
# #' @export
# display_scores <- function(scores,
# 									year,
# 									doy,
# 									complete_year=TRUE,
# 									which_year=2000,
# 									which_images=NULL,
# 									n_show,
# 									save_it=FALSE,
# 									save_file=NULL,
# 									main=NULL,
# 									sep_plots=FALSE
# ){
# 	
# 	if(save_it & length(which_year)>1){
# 		stop("For saving, only one year is possible by now.")
# 	}
# 	
# 	if(save_it){
# 		if(!file.exists(save_file)) dir.create(save_file)
# 	}
# 	
# 	n_years <- length(which_year)
# 	n_pc <- nrow(scores)
# 	
# 	# Average scores per doy
# 	scores_doy <- vector("list",length=n_years)
# 	
# 	for(i in seq_len(n_years)){
# 		
# 		if(complete_year){
# 			
# 			# Which frames correspond to this year?
# 			which_frame <- which(year==which_year[i])
# 			
# 		}else{
# 			which_frame <- which_images
# 		}
# 		
# 		# For which doys there are frames to be shown?
# 		which_doy <- unique(doy[which_frame])
# 		
# 		# Initialize matrix for averaged scores
# 		scores_doy[[i]] <- matrix(nrow=length(which_doy),
# 										  ncol=n_pc)
# 		rownames(scores_doy[[i]]) <- which_doy
# 		
# 		# Compute averaged scores
# 		for(j in seq_len(n_pc)){
# 			scores_doy[[i]][,j] <- tapply(X = scores[j,which_frame], 
# 													INDEX = doy[which_frame], 
# 													FUN = mean)
# 		}
# 	}
# 	
# 	################################################
# 	# Plot averaged scores + lowess-estimator
# 	################################################
# 	
# 	# Plot 
# 	if(save_it){
# 		if(sep_plots){
# 			filename <- paste0(save_file,"scores_sep.pdf")
# 		}else{
# 			filename <- paste0(save_file,"scores.pdf")
# 		}
# 		pdf(file=filename,
# 			 width=12,
# 			 height=8)
# 	}
# 	
# 	if(!sep_plots){
# 		par(mfrow=n2mfrow(n_show*n_years))
# 	}
# 	
# 	for(i in seq_len(n_years)){
# 		for(j in seq_len(n_show)){
# 			
# 			if(is.null(main)){
# 				main_plot <- which_year[i]
# 			}else{
# 				main_plot <- main[j]
# 			}
# 			
# 			if(sep_plots){
# 				par(mfrow=c(1,1))
# 			}
# 			
# 			plot(rownames(scores_doy[[i]]),scores_doy[[i]][,j],
# 				  type="l", col=1,lwd=1, 
# 				  xlab="DOY", ylab=paste0(j,". PC"), main=main_plot)
# 			lines(lowess(rownames(scores_doy[[i]]),scores_doy[[i]][,j],f = .1),
# 					col="blue", lwd=2, lty=2)
# 			
# 		}
# 	}
# 	
# 	if(save_it) dev.off()
# 	
# }
