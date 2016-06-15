##############################################
# Package phenofun
# Code for reading in the data from AMOS (http://amos.cse.wustl.edu/)
# 
# Date: 15.06.16
# Author: Ludwig Bothmann
##############################################

# Set working directory to path where to save the data
setwd("data/amos/")
getwd()

# Define name of camera, years and months to be downloaded
camera <- 8568
year <- 2014
month <- 2

download_amos_func(camera=camera, year=year, month=month)