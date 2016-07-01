##############################################
# Eigenanalysis and supervised classification
# 
# Date: 01.07.16
# Autoren: Ludwig Bothmann
##############################################

rm(list=ls())

setwd("../../../../Desktop/sroi_test/")

# Load package "phenofun"
library(devtools)
load_all("../../Dokumente/rforge/phenofun/pkg")

# # Install package phenofun from R-Forge
# install.packages("phenofun", repos="http://R-Forge.R-project.org")
# 
# # Load package "phenofun"
# library(phenofun)
# # help(package="phenofun")

#############################################################################
# Set parameters
#############################################################################


# Which parts of the analysis shall be carried out?
do_eigenanalysis <- TRUE
do_classification <- TRUE # Muss auch für do_classification_newyear TRUE sein
do_classification_newyear <- FALSE

# Folder of results
folder_results <- "results/"

# Folder of data
folder_data <- "../../Dokumente/Phenology/data/WebCam/cam7/"

# Name of analyis (for path of results)
name_of_analysis <- "sup_class_2014"

# Dimensions of image
x <- 1285:2560
y <- 1:960

# Vector with file names of relevant images
lists_files <- list.files(path=folder_data)[-(1:8)]

# Extract DOYs and years
year <- as.numeric(substr(lists_files,12,15))
doy <- as.numeric(substr(lists_files,26,28))

# Training year
training_year <- 2014

# Test year
test_year <- 2013

# True season classes
spring <- 100:122
summer <- 123:279
autumn <- 280:308
winter <- c(1:99,309:365)

season_list <- list(spring=spring,
                    summer=summer,
                    autumn=autumn,
                    winter=winter)

complete_year <- TRUE
n_images_try <- 20
  
# Number of principal components
n_pc <- 12

#############################################################################
# Carry out analysis
#############################################################################

# Print sessionInfo and save time of start
sessionInfo()
time_start <- Sys.time()

sup_class(folder_results=folder_results,
          folder_data=folder_data,
          name_of_analysis=name_of_analysis,
          lists_files=lists_files,
          do_eigenanalysis=do_eigenanalysis,
          do_classification=do_classification,
          do_classification_newyear=do_classification_newyear,
          x=x,
          y=y,
          training_year=training_year,
          test_year=test_year,
          season_list=season_list,
          n_pc=n_pc,
          year=year,
          doy=doy,
          complete_year=complete_year,
          n_images_try=n_images_try)

print(gc())
time_end <- Sys.time()
print(time_end - time_start)