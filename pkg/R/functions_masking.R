###########################################################################
# Functions for masking
# Date: 18.01.16
# Author: Ludwig Bothmann
###########################################################################

###################################
# mask_img()
###################################

#' Masks images with given mask
#' 
#' Masks images with given mask, i.e., sets pixel values to 0 which are not in the mask
#' 
#' @param images array of pixel intensities, 3rd dimension has to be the
#' 	color channels, 4th dimension the time. For grayscale images this
#' 	means that \code{dim(images) = (*,*,1,*)}, for color images 
#' 	\code{dim(images) = (*,*,3,*)}
#' @param mask matrix with 0/1 giving the mask
#' @return Array with same dimension as input array.
#' @export
mask_img <- function(images,
							mask){
	
	images_flat <- matrix(images, nrow=dim(images)[1])
	
	mask_long <- matrix(mask, nrow=dim(images)[1],
							  ncol=prod(tail(dim(images),-1)))
	
	images_flat_mask <- images_flat * mask_long
	
	images <- array(images_flat_mask, dim=dim(images))
}