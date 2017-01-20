###########################################################################
# Functions for input of images
# Date: 18.01.16
# Author: Ludwig Bothmann
###########################################################################

#' Read images into an array
#' 
#' This function reads in a set of images and returns all images as an array.
#' 
#' @param colormode the output colormode: either "Color" (default) or "Grayscale"
#' @param dim_images the dimension of the output array
#' @param folder the path of the folder which contains the images
#' @param file_names a vector giving the names of the input files
#' @param which_images indices of the images to read, 
#' 	relative to \code{file_names} 
#' @param x Vector of x-coordinates to read in. Default \code{NULL} results in 
#'  a grid of all possible values.
#' @param y Vector of y-coordinates to read in. Default \code{NULL} results in 
#'  a grid of all possible values.
#' @param col_out which color-channels shall be read in?
#' @param col_in which color-channels has the input image?
#' @param sum1 \code{TRUE}: Pixel intensities are normed to sum=1, i.e. 
#' 	relative intensities are computed, default is \code{FALSE}
#' @param norm_it \code{TRUE}: Images are normed so that column, row and 
#' 	colour channel intensities have sum=1 or mean=0, default is \code{FALSE}
#' @param norm_type Type of normalization, either sum of each image = 1, 
#' 	i.e., \code{"sum1"}, or mean of each image = 0, i.e., \code{"mean0"},
#' 	default \code{"sum1"}
#' @param colorspace either \code{"rgb"} or \code{"hsv"}, 
#' 	default is \code{"rgb"}
#' @param relative \code{TRUE}: If only one colour channel shall be extracted
#' 	from a colour image, then this channel is returned relatively to the
#' 	other two channels, default is \code{FALSE}. 
#' @return An array containing the images. If \code{sum1=TRUE}, then the 
#' 	third colour channel is dropped to save memory.
#' @importFrom EBImage readImage
#' @importFrom EBImage Image
#' @importFrom EBImage imageData
#' @export
read_images <- function(colormode="Color", 
								dim_images=NULL, 
								folder,
								file_names, 
								which_images,
								x=seq_len(dim_images[1]), 
								y=seq_len(dim_images[2]), 
								col_out=seq_len(dim_images[3]),
								col_in,
								sum1=FALSE,
								norm_it=FALSE,
								norm_type="sum1",
								colorspace="rgb",
								relative=FALSE){
	
	# 	if(length(x)!=dim[1] |
	# 			length(y)!=dim[2] |
	# 			length(col_out)!=dim[3] |
	# 			length(which_images)!=dim[4]){
	# 	
	# 		stop("Dimension of the output array and desired input do not fit.")
	# 	}
	
	if(!(colorspace=="rgb"|colorspace=="hsv")){
		stop("Please select rgb- or hsv-space\n")
	}
	
	if(colorspace!="rgb" & norm_it){
		stop("Until now, norm_it=TRUE only available for rgb-space\n")
	}
	
	if(sum1 & norm_it){
		stop("sum1=TRUE and norm_it=TRUE: I don't know what to do!")
	}
	
	if(colormode=="Grayscale" & sum1){
		stop("It does not make sense to standardize a Grayscale image to sum=1 \n")
	}
	
	# If no dimensions are specified or just one dimension is specified: read the remaining dimensions 
	# from the image itself
	if(is.null(dim_images) & (is.null(x) | is.null(y))){
		
		img_dim <- readImage(paste0(folder,file_names[1]))
		if(is.null(x))	x <- seq_len(dim(img_dim)[1])
		if(is.null(y)) y <- seq_len(dim(img_dim)[2])
		
		rm(list="img_dim")
		gc()
	}
	
	dim_images <- c(length(x), 
						 length(y), 
						 length(col_out), 
						 length(which_images))
	
	# 	if(sum1){
	# 		dim_sum1 <- dim_images
	# 		dim_sum1[3] <- 2
	# 	}
	
	if(length(col_in)==1 & colormode=="Color"){
		warning("Are you sure you want to read only 1 color channel but
				  save it as Color image? ")
	}
	
	images <- array(dim=dim_images)
	
	if(colormode == "Color"){
		
		for(i in seq_along(which_images)){
			
			if(i %% 50 == 1){ 
				cat("Index ",i,": Image ", which_images[i], " - ", 
					 file_names[which_images[i]], "\n")
			}
			images[,,,i] <- imageData(readImage(
				paste0(folder,file_names[which_images[i]]))[x,y,col_in])
			
		}
		
		if(sum1){
			
			images <- sum1_img(images)
			
			# 			# Rearrange dimensions to have a matrix with 3 columns
			# 			images_mat <- array(images, 
			# 									  dim=c(prod(head(dim_images, 2),
			# 									  			  tail(dim_images, 1)),
			# 									  		dim(images)[3]))
			# 			
			# 			# Norm this matrix to sum=1
			# 			images_norm_mat <- images_mat / rowSums(images_mat)
			# 			
			# 			# Black Pixels have intensity 0 in each channel and sum 0, 
			# 			#	i.e. division yields NaN
			# 			# Set those Pixels to black again
			# 			images_norm_mat[which(is.nan(images_norm_mat))] <- 0
			# 			
			# 			# ... and rearrange it as before, dropping column 3
			# 			images <- aperm(array(images_norm_mat[,1:2],
			# 										 dim=dim_sum1[c(1,2,4,3)]),
			# 								 c(1,2,4,3))
			
			# 			# Less memory using this but without removing the NAs
			# 			images <- aperm(array((images_mat / rowSums(images_mat))[,1:2],
			# 												dim=dim_sum1[c(1,2,4,3)]),
			# 										c(1,2,4,3))
			
		}else if(norm_it){
			
			images <- norm_img(images,
									 norm_type=norm_type)
			
			# 			# Rearrange dimensions to have a matrix 
			# 			#	with length(which_images) columns
			# 			images_mat <- array(images, 
			# 									  dim=c(prod(head(dim_images, 3)),
			# 									  		dim_images[4]))
			# 			
			# 			# Norm columns of this matrix to sum=1
			# 			images_norm_mat <- t(t(images_mat) / colSums(images_mat))
			# 		
			# 			# ... and rearrange it as before
			# 			images <- array(images_norm_mat,
			# 								 dim=dim_images)
			
		}else if(colorspace=="hsv"){
			
			images <- rgb_to_hsv(images)
			
			# 			# Rearrange dimensions to have a matrix with 3 rows
			# 			images_mat <- t(array(images, 
			# 									  dim=c(prod(head(dim_images, 2),
			# 									  			  tail(dim_images, 1)),
			# 									  		dim_images[3])))
			# 			
			# 			# Transfrom rgb-values into hsv-colorspace
			# 			hsv_mat <- rgb2hsv(images_mat,maxColorValue=1)
			# 		
			# 			# ... and rearrange it as before
			# 			images <- aperm(array(t(hsv_mat),
			# 								 dim=dim_images[c(1,2,4,3)]),
			# 								 c(1,2,4,3))
			
		}
		
	}else if (colormode == "Grayscale"){
		if(!relative){
			
			for(i in seq_along(which_images)){
				
				if(i %% 50 == 1){ 
					cat("Index ",i,": Image ", which_images[i], " - ", 
						 file_names[which_images[i]], "\n")}
				# 				images[,,,i] <- imageData(channel(readImage(
				# 					paste0(folder,file_names[which_images[i]]))[x,y,col_in],
				# 					mode="luminance"))
				# 				The above is wrong and norms the channel in some way
				# 				see ?channel and ?Image: With the above he seems to assume
				# 				that this color channel is a red channel and multiplies
				# 				everything with .2126
				# 				what I want is only changing the colorMode, not changing the
				# 				intensities
				images[,,,i] <- imageData(Image(readImage(
					paste0(folder,file_names[which_images[i]]))[x,y,col_in],
					colormode="Grayscale"))
			}
		}else{
			
			for(i in seq_along(which_images)){
				
				if(i %% 50 == 1){ 
					cat("Index ",i,": Image ", which_images[i], " - ", 
						 file_names[which_images[i]], "\n")}
				
				# Read all colour channels
				img <- imageData(readImage(paste0(folder,file_names[which_images[i]]))[x,y,1:3])
				
				# Norm to sum = 1
				img_flat <- array(data=img,dim=c(length(x)*length(y),3))
				img_flat_norm <- img_flat/rowSums(img_flat)
				img_array <- array(img_flat_norm,dim=c(length(x),length(y),3))
				
				# Only save colour channel "col_in"
				images[,,,i] <- img_array[,,col_in]
			}
			
		}
	}
	
	return(images)
}


#' Function to read images into an array -- NDVI Version
#' 
#' @param folder the path of the folder which contains the images
#' @param file_names a vector giving the names of the input files
#' @param which_images indices of the images to read, 
#' 	relative to \code{file_names} 
#' @param x_ir which x-values shall be read in for the infrared-channel?
#' @param y_ir which y-values shall be read in for the infrared-channel?
#' @param x_red which x-values shall be read in for the red-channel?
#' @param y_red which y-values shall be read in for the red-channel?
#' @return An array containing the images.
#' @importFrom EBImage readImage
#' @importFrom EBImage imageData
#' @importFrom EBImage Image
read_images_ndvi <- function(folder,
									  file_names, 
									  which_images,
									  x_ir, 
									  y_ir, 
									  x_red,
									  y_red){
	
	dim_images <- c(length(x_ir), 
						 length(y_ir), 
						 1, 
						 length(which_images))
	
	images_ir <- array(dim=dim_images)
	images_red <- array(dim=dim_images)
	
	for(i in seq_along(which_images)){
		
		if(i %% 50 == 1){ 
			cat("Index ",i,": Image ", which_images[i], " - ", 
				 file_names[which_images[i]], "\n")
		}
		
		# IR-Bild
		images_ir[,,,i] <- imageData(Image(readImage(
			paste0(folder,file_names[which_images[i]]))[x_ir,y_ir,1],
			colormode="Grayscale"))
		
		# Rot-Bild
		images_red[,,,i] <- imageData(Image(readImage(
			paste0(folder,file_names[which_images[i]]))[x_red,y_red,1],
			colormode="Grayscale"))
	}
	
	# NDVI = (IR-RED)/(IR+RED)
	images <- (images_ir - images_red)/(images_ir + images_red)
	
	# Division by 0 makes NaN => set to 0
	images[which(is.nan(images))] <- 0
	
	return(images)
}


#' Function to read images into an array -- Pseudo-NDVI Version
#' 
#' @param folder the path of the folder which contains the images
#' @param file_names a vector giving the names of the input files
#' @param which_images indices of the images to read, 
#' 	relative to \code{file_names} 
#' @param x which x-values shall be read in?
#' @param y which y-values shall be read in?
#' @return An array containing the images.
#' @importFrom EBImage readImage
#' @importFrom EBImage imageData
#' @importFrom EBImage Image
read_images_pseudo_ndvi <- function(folder,
												file_names, 
												which_images,
												x, 
												y){
	
	dim_images <- c(length(x), 
						 length(y), 
						 1, 
						 length(which_images))
	
	images_green <- array(dim=dim_images)
	images_red <- array(dim=dim_images)
	
	for(i in seq_along(which_images)){
		
		if(i %% 50 == 1){ 
			cat("Index ",i,": Image ", which_images[i], " - ", 
				 file_names[which_images[i]], "\n")
		}
		
		# Gruen-Bild
		images_green[,,,i] <- imageData(Image(readImage(
			paste0(folder,file_names[which_images[i]]))[x,y,2],
			colormode="Grayscale"))
		
		# Rot-Bild
		images_red[,,,i] <- imageData(Image(readImage(
			paste0(folder,file_names[which_images[i]]))[x,y,1],
			colormode="Grayscale"))
	}
	
	# Pseudo-NDVI = (GREEN-RED)/(GREEN+RED)
	images <- (images_green - images_red)/(images_green + images_red)
	
	# Division by 0 makes NaN => set to 0
	images[which(is.nan(images))] <- 0
	
	return(images)
}



###################################
# rgb_to_hsv()
###################################

#' Transform RGB-Image to HSV-Image
#' 
#' This function transforms a given RGB-Image (as array) into an 
#' 	HSV-Image (as array)
#' 	
#' @param images Image as 4D or 3D array
#' @param maxColorValue Maximum color value, default=1
#' @return 4D or 3D array of same dimension as input but with hsv values

rgb_to_hsv <- function(images,
							  maxColorValue=1
){
	
	dim_images <- dim(images)
	
	# 	if(length(dim_images)!=4){
	# 		stop("input has to be a 4D array")
	# 	}
	
	if(length(dim_images)==4){
		
		# Rearrange dimensions to have a matrix with 3 rows
		images_mat <- t(array(aperm(images,c(1,2,4,3)), 
									 dim=c(prod(head(dim_images, 2),
									 			  tail(dim_images, 1)),
									 		dim_images[3])))
		
		# Transform rgb-values into hsv-colorspace
		hsv_mat <- rgb2hsv(images_mat,maxColorValue=maxColorValue)
		
		# ... and rearrange it as before
		images <- aperm(array(t(hsv_mat),
									 dim=dim_images[c(1,2,4,3)]),
							 c(1,2,4,3))
		
		return(images)
		
	}else if(length(dim_images)==3){
		
		# Rearrange dimensions to have a matrix with 3 rows
		images_mat <- t(array(images, 
									 dim=c(prod(head(dim_images, 2)),
									 		dim_images[3])))
		
		# Transform rgb-values into hsv-colorspace
		hsv_mat <- rgb2hsv(images_mat,maxColorValue=maxColorValue)
		
		# ... and rearrange it as before
		images <- array(t(hsv_mat),
							 dim=dim_images)
		
		return(images)
		
	}else{
		stop("input has to be a 4D or 3D array")
	}
}

###################################
# hsv_to_rgb()
###################################

#' Transform HSV-Image to RGB-Image
#' 
#' This function transforms a given HSV-Image (as array) into an 
#' 	RGB-Image (as array)
#' 	
#' @param images Image as 4D or 3D array
#' @return 4D or 3D array of same dimension as input but with rgb values

hsv_to_rgb <- function(images){
	
	dim_images <- dim(images)
	
	# 	if(length(dim_images)!=4){
	# 		stop("input has to be a 4D array")
	# 	}
	
	if(length(dim_images)==4){
		
		# Rearrange dimensions to have a matrix with 3 rows
		img_hsv_mat <- t(array(aperm(images,c(1,2,4,3)), 
									  dim=c(prod(head(dim_images, 2),
									  			  tail(dim_images, 1)),
									  		dim_images[3])))
		
		# Transform hsv-values into rgb-colorspace
		rgb_mat <- col2rgb(hsv(h=img_hsv_mat[1,],
									  s=img_hsv_mat[2,],
									  v=img_hsv_mat[3,]))
		
		# ... and rearrange it as before
		images <- aperm(array(t(rgb_mat),
									 dim=dim_images[c(1,2,4,3)]),
							 c(1,2,4,3))
		
		return(images)
		
	}else if(length(dim_images)==3){
		
		# Rearrange dimensions to have a matrix with 3 rows
		img_hsv_mat <- t(array(images, 
									  dim=c(prod(head(dim_images, 2)),
									  		dim_images[3])))
		
		# Transform hsv-values into rgb-colorspace
		rgb_mat <- col2rgb(hsv(h=img_hsv_mat[1,],
									  s=img_hsv_mat[2,],
									  v=img_hsv_mat[3,]))
		
		# ... and rearrange it as before
		images <- array(t(rgb_mat),
							 dim=dim_images)
		
		return(images)
		
	}else{
		stop("input has to be a 4D or 3D array")
	}
}

###################################
# norm_img()
###################################

#' Norm image to sum=1
#' 
#' This function norms an image to sum=1 or mean=0 over rows, columns and color
#' 	channels
#' 	
#' @param images Image as 4D array
#' @param norm_type sum1 or mean0
#' @return 4D array of same dimension as input but with normed values
#' @export

norm_img <- function(images,
							norm_type="sum1"){
	
	dim_images <- dim(images)
	
	if(length(dim_images)!=4){
		stop("input has to be a 4D array")
	}
	
	# Rearrange dimensions to have a matrix 
	#	with length(which_images) columns
	images_mat <- array(images, 
							  dim=c(prod(head(dim_images, 3)),
							  		dim_images[4]))
	
	if(norm_type=="sum1"){
		
		# Norm columns of this matrix to sum=1
		images_norm_mat <- t(t(images_mat) / colSums(images_mat))
		
	}else if(norm_type=="mean0"){
		
		# Norm columns of this matrix to mean=0
		images_norm_mat <- t(t(images_mat) - colMeans(images_mat))
		
	}else{
		stop("norm_img: Either of norm_type=sum1 or mean0 is expected")
	}
	
	# ... and rearrange it as before
	images <- array(images_norm_mat,
						 dim=dim_images)
	
	return(images)
}


###################################
# sum1_img()
###################################

#' Transform pixel intensities to sum=1
#' 
#' This function transforms the pixel intensities of an image to sum=1
#' 	(only over the color channel, contrary to norm_img)
#' 	
#' @param images Image as 4D array
#' @return 4D array of transformed values, the third color channel is
#' 	dropped to avoid running into collinearity problems with the
#' 	eigenanalysis
#' @export

sum1_img <- function(images){
	
	
	dim_images <- dim(images)
	
	if(length(dim_images)!=4){
		stop("input has to be a 4D array")
	}
	
	dim_sum1 <- dim_images
	dim_sum1[3] <- 2
	
	# Rearrange dimensions to have a matrix with 3 columns
	images_mat <- array(aperm(images,c(1,2,4,3)), 
							  dim=c(prod(head(dim_images, 2),
							  			  tail(dim_images, 1)),
							  		dim(images)[3]))
	
	# Norm this matrix to rowsums=1
	images_norm_mat <- images_mat / rowSums(images_mat)
	
	# Black Pixels have intensity 0 in each channel and sum 0, 
	#	i.e. division yields NaN
	# Set those Pixels to black again
	# 	images_norm_mat[which(is.nan(images_norm_mat))] <- 0
	
	# ... and rearrange it as before, dropping column 3
	images <- aperm(array(images_norm_mat[,1:2],
								 dim=dim_sum1[c(1,2,4,3)]),
						 c(1,2,4,3))
	
	# 			# Less memory using this but without removing the NAs
	# 			images <- aperm(array((images_mat / rowSums(images_mat))[,1:2],
	# 												dim=dim_sum1[c(1,2,4,3)]),
	# 										c(1,2,4,3))
	
	return(images)
}


###################################
# scale_img()
###################################

#' Scale grayscale images to mean 0 and variance 1
#' 
#' @param images array of pixel intensities, 3rd dimension has to be the
#' 	color channels, 4th dimension the time. For grayscale images this
#' 	means that \code{dim(images) = (*,*,1,*)}, for color images 
#' 	\code{dim(images) = (*,*,3,*)}
#' @param center \code{TRUE} (default): Images are centered
#' @param scale \code{TRUE} (default): Images are standardized to variance 1
#' @import irlba
#' @return The scaled images as array
#' @export
scale_img <- function(images,
							 center=TRUE,
							 scale=TRUE){
	
	# Dimension of the images array
	dim_images <- dim(images)
	
	# Flat: Matrix with 
	#		#columns = #images
	#		#rows = #pixels
	images_flat <- array(images, 
								dim=c(prod(head(dim_images, 2)),
										tail(dim_images, 1)))
	
	# Compute scaled image
	images_flat_scaled <- scale(t(images_flat),
										 center=center,
										 scale=scale)
	
	cat(as.character(Sys.time()),"Scaling done \n")
	
	# Rearrange scaled image in array
	images_scaled <- array(t(images_flat_scaled),
								  dim=dim_images)
	
	return(images_scaled)
}
