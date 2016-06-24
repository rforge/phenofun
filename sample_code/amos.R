##############################################
# Package phenofun
# sample code for uroi analysis for data from amos (http://amos.cse.wustl.edu/)
#
# Date: 24.06.16
# Author: Ludwig Bothmann
##############################################

rm(list=ls())

# Set working directory regarding to your folder structure
# setwd("/home/bothmannlu/Dokumente/Phenology")

setwd("/home/bothmannlu/Desktop/amos_test")

# Load package "phenofun"
# library(devtools)
# load_all("/home/bothmannlu/Dokumente/rforge/phenofun/pkg")
install.packages("phenofun", repos="http://R-Forge.R-project.org")
library(phenofun)
help(package="phenofun")

# Print sessionInfo and time of start
sessionInfo()
(time_start <- Sys.time())

# Specify here the IDs of the cameras to be analyzed...
camera_vec <- 1:10
# ... the respective year...
year_analysis_vec <- rep(2015,length(camera_vec))
# ... the respective months ...
months_analysis_list <- vector("list",length(camera_vec))
for(i in 1:length(camera_vec)){
  months_analysis_list[[i]] <- 1:12
}
# ... and the hour to start
hour_analysis_vec <- rep(17, length(camera_vec))

# Base directory for the results
folder_results <- "results/amos"

# Directory of images
folder_data <- "data/amos"

# If TRUE, only 10 images are analyzed
testmode <- TRUE

# This vector is TRUE for cameras where no error occurred
no_error <- rep(NA,length(camera_vec))

# In this vector, the running time is saved
runtime <- rep(NA,length(camera_vec))

dir.create(folder_results, recursive = TRUE)
dir.create(folder_data, recursive = TRUE)

###################################
# Here the analysis start
###################################

# Analyse all cameras
for(i in 1:length(camera_vec)){
  
  cat("i =",i,"\n")
  start_i <- as.numeric(Sys.time())
  camera <- camera_vec[i]
  year_analysis <- year_analysis_vec[i]
  months_analysis <- months_analysis_list[[i]]
  hour_analysis <- hour_analysis_vec[i]
  
  # Transform number of camera in 5-digit string
  camera_ch <- as.character(camera)
  camera_5digit <- substr(paste0("0000",camera_ch),nchar(camera_ch),5+nchar(camera_ch))
  
  # Base directory for the results
  path_base <- paste0(folder_results,"/000",camera_5digit,"/")
  
  # Analysis only if not analyzed before
  if(!file.exists(path_base)){
    
    # Directory of images
    folder <- paste0(folder_data,"/AMOS_Data/000",camera_5digit,"/")
    
    no_error[i] <- is.null(try(amos_uroi_wrap(camera=camera,
                                              year_analysis=year_analysis,
                                              months_analysis=months_analysis,
                                              hour_analysis=hour_analysis,
                                              testmode=testmode,
                                              folder_results=folder_results,
                                              folder_data=folder_data),
                               silent=FALSE))
    
    # Delete images after analysis
    system(paste0("rm -rf ",folder))
    
    if(!no_error[i]){
      
      # If an error occurrde: Delete folder of results
      system(paste0("rm -rf ",path_base))
      
    }
    
    end_i <- as.numeric(Sys.time())
    runtime[i] <- end_i - start_i
    
  }else{
    
    # If path exists this means that the analysis could be carried out before
    no_error[i] <- TRUE
  }
  
  # In the resulting .csv, an overview of past analyses is saved
  success <- data.frame(camera_vec, 
                        no_error=no_error, 
                        runtime=runtime)
  
  write.csv(success, file=paste0("results/amos/success_",camera_vec[1],
                                 "_to_",camera_vec[length(camera_vec)],".csv"))
}

(time_end <- Sys.time())

print(time_end - time_start)
