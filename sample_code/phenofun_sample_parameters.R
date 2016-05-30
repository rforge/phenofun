##############################################
# Package phenofun
# sample code for uroi analysis
# part 1: parameter setting
# 
# Date: 30.05.16
# Author: Ludwig Bothmann
##############################################

do_mean_greenness <- TRUE
do_strucchange <- TRUE

# Testmode: Only 10 images and few settings
testmode <- FALSE

# Name of analysis (perhaps with year of used data)
# => Only for file names of saved objects, can be anything
name_of_analysis <- "160530_marquartstein"

if(testmode){
  
  # Which images shall be analysed
  complete_year <- FALSE
  year_analysis <- 2013 # only relevant if complete_year=TRUE
  n_images_try <- 10 # 834 # only relevant if complete_year=FALSE
  
  # Parameters for the clusteranalysis
  n_pc_vec <- 2
  k_vec <- 2:3
  
  # Iterations of kmeans
  nstart <- 1
  
}else{
  
  # Which images shall be analysed
  complete_year <- TRUE
  year_analysis <- 2013 # only relevant if complete_year=TRUE
  n_images_try <- 10 # 834 # only relevant if complete_year=FALSE
  
  # Parameters for the clusteranalysis
  n_pc_vec <- c(12,24)# c(4,8,12,16,20) #8 #c(12,24)#2 # c(12,36) # NULL
  k_vec <- 4:10#2:3##4:15#4:6
  
  # Iterations of kmeans
  nstart <- 2#1
  
}

set.seed(1112)

# Base directory for the results (has to exist)
path_base <- "../../Phenology/results/phenofun_test/"

# Directory containing the images
folder <- "../../Phenology/data/foto-webcam/marquartstein/"


# Vector with file names of relevant images
lists_files <- list.files(path=folder)
# lists_files <- list.files(path=folder)[-(1:8)]

# Extract year and doy from file name...
doy <- c(1:365)
year <- as.numeric(substr(lists_files,1,4))
# year <- as.numeric(substr(lists_files,12,15))
# doy <- as.numeric(substr(lists_files,26,28))
# # ... or specify it directly, for example:
# year <- rep(year_analysis, length(lists_files))
# doy <- 1:365

# Title for plot
main_plot <- paste0("Marquartstein - ",year_analysis)
# main_plot <- paste0("Kranzberg 2 - ",year_analysis)

# Pixels of the images which shall be considered 
#	(x- and y-coordinates, only rectangles possible by now)
# If not specified: Whole image is analysed
x <- 1:1200
y <- 51:675
# x <- 1285:2560
# y <- 1:960

if(complete_year){
	
	# Only images of year "year_analysis"
	which_images <- seq_len(length(lists_files))[which(year%in%year_analysis)]
	
}else{
	
	# Only first n_images_try images in the folder
	which_images <- seq_len(n_images_try)	
}

# # you can define which_images in any way, for example for 50 images:
# which_images <- round(seq(1,length(lists_files), length=50))
# # ... or for each 10. image:
# which_images <- round(seq(1,length(lists_files), by=10))

# which_images <- which_images[seq(1,length(which_images), length=100)]
