##############################################
# Automated sROI-approach
# 
# Date: 01.07.16
# Autoren: Ludwig Bothmann
##############################################

#################################################################
#	0. Set parameters
#################################################################

rm(list=ls())

setwd("/home/bothmannlu/Desktop/sroi_test/")

library("EBImage")
library("irlba")
library("strucchange")

# Load package "phenofun"
library(devtools)
load_all("../../Dokumente/rforge/phenofun/pkg")

# # Install package phenofun from R-Forge
# install.packages("phenofun", repos="http://R-Forge.R-project.org")
# 
# # Load package "phenofun"
# library(phenofun)
# # help(package="phenofun")

# Which parts of the analysis shall be carried out?
get_timeseries <- TRUE
correlate_timeseries <- TRUE
do_evaluation <- TRUE
do_strucchange <- TRUE

# Color channel of interest
color <- "green"

# Name of analyis (for path of results)
name_of_analysis <- "160701"

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

# Number of random pinpricks
n_regions <- 2

# Vector with thresholds for correlations
threshold_vec <- seq(.5, .9, length=2)#by=.05)

# Set seed
seed <- 1014

#################################################################
#	1. Carry out sroi analysis
#################################################################

# Print sessionInfo and save time of start
sessionInfo()
time_start <- Sys.time()

sroi_analysis(get_timeseries=get_timeseries,
              correlate_timeseries=correlate_timeseries,
              do_evaluation=do_evaluation,
              do_strucchange=do_strucchange,
              color=color,
              name_of_analysis=name_of_analysis,
              folder_results=path_base,
              folder_data=folder,
              main_camera=main_camera,
              n_pinpricks=n_regions,
              seed=seed,
              threshold_vec=threshold_vec,
              lists_files=lists_files,
              which_images=which_images,
              doy=doy,
              doy_c=doy_c)

print(gc())
time_end <- Sys.time()
print(time_end - time_start)