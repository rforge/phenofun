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
