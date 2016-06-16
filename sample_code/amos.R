##############################################
# Package phenofun
# sample code for uroi analysis for data from amos (http://amos.cse.wustl.edu/)
#
# Date: 16.06.16
# Author: Ludwig Bothmann
##############################################

rm(list=ls())

# Set working directory regarding to your folder structure
setwd("/home/bothmannlu/Dokumente/Phenology")

# Load package "phenofun"
library(devtools)
load_all("../rforge/phenofun/pkg")

# Print sessionInfo and time of start
sessionInfo()
(time_start <- Sys.time())

# Read in selection
selection <- read.csv("data/amos/selection.csv")
part <- 1
rows <- seq(part*6-5,part*6)

# Define name of camera, years and months to be downloaded
camera_vec <- selection[rows,1]
year_analysis_vec <- selection[rows,2]
months_analysis_list <- vector("list")
for(i in 1:length(camera_vec)){
  months_analysis_list[[i]] <- seq(selection[i,3],selection[i,4])
}
hour_analysis_vec <- selection[rows,5]


# Analyse all cameras
for(i in 6){    #1:length(camera_vec)){
  
  cat("i =",i,"\n")
  camera <- camera_vec[i]
  year_analysis <- year_analysis_vec[i]
  months_analysis <- months_analysis_list[[i]]
  hour_analysis <- hour_analysis_vec[i]
  
  amos_uroi_wrap(camera=camera,
                 year_analysis=year_analysis,
                 months_analysis=months_analysis,
                 hour_analysis=hour_analysis,
                 testmode=FALSE,
                 folder_results="results/amos",
                 folder_data="data/amos")
}

(time_end <- Sys.time())

print(time_end - time_start)
