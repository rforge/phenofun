##############################################
# Package phenofun
# sample code for uroi analysis
# part 2: uroi analysis 
#
# Date: 30.05.16
# Author: Ludwig Bothmann
##############################################

#################################################################
#	0. Set parameters
#################################################################

rm(list=ls())

# Set working directory regarding to your folder structure
setwd("/home/bothmannlu/Dokumente/rforge/phenofun")

# Load package "phenofun"
library(devtools)
load_all("pkg")

sessionInfo()
Sys.time()

# # Alternatively load package from R-Forge
# install.packages("phenofun",
#                  repos="http://R-Forge.R-project.org")
# 
# # ... or from tar.gz
# install.packages("phenofun_0.0.4.tar.gz", repos=NULL, type="source")
# library(phenofun)



# source file with parameters
filename_parameters <- "sample_code/phenofun_sample_parameters.R"

source(filename_parameters)

# create directories if not done manually
create_dirs_uroi(path_base=path_base,
                 name_of_analysis=name_of_analysis)

#################################################################
#	1. Find clusters in the images
#################################################################

clusters_out <- find_clusters(folder=folder, 
                              lists_files=lists_files, 
                              which_images=which_images, 
                              n_pc_vec=n_pc_vec, 
                              k_vec=k_vec, 
                              path_base=path_base, 
                              name_of_analysis=name_of_analysis,
                              save_results=TRUE
                              # ,colormode="Grayscale"
                              ,x=x,y=y,
                              nstart=nstart
)

#################################################################
#	2. Generate masks
#################################################################

masks_list <- generate_masks(settings=clusters_out$settings,
                             cluster_list=clusters_out$cluster_list,
                             name_of_analysis=name_of_analysis,
                             path_base=path_base,
                             return_masks = TRUE,
                             save_masks=TRUE,
                             type=".png")

# # Overlay of masks with reference image
# # Specify reference image
# ref_image <- readImage(paste0(folder,lists_files[1]))
# ref_image <- ref_image[x,y,]
# # Compute overlay
# overlay <- overlay_masks_onref(masks_list, ref_image, alpha=.3)
# # display(overlay)
# 
# # and save overlay
# writeImage(overlay,files=paste0(path_base,name_of_analysis,"/masks/overlay.jpg"))

####################################################################
#	3. Mask images and compute mean %greenness time series (per doy)
####################################################################

if(do_mean_greenness){
  
  mean_greenness <- compute_greenness_time_series(color="green",
                                                  folder=folder,
                                                  lists_files=lists_files,
                                                  which_images=which_images,
                                                  doy=doy,
                                                  path_base=path_base,
                                                  name_of_analysis=name_of_analysis,
                                                  main_plot=main_plot,
                                                  settings_mat=clusters_out$settings_mat,
                                                  load_masks=FALSE,
                                                  masks_list=masks_list)
}

#################################################################
#	4. Analysis of structural changes
#################################################################

# Note: you need more data than the example data to carry out an analysis of
#	structural changes

if(do_strucchange){
  
  # take \%greenness from step 3
  mean_doy_norm <- mean_greenness$mean_doy_norm
  settings_mat <- mean_greenness$settings_mat
  
  # # or load it
  # load("results/lackenberg/mask_timeseries/uroi/160130_2014_4_gutebilder/mean_doys_uROI_green.RData")
  # load("results/lackenberg/160201/mask_timeseries/mean_doys_uROI_green.RData")
  # # Results paper:
  # load("results/kranzberg2/comparison_rel_green/mask_timeseries/151013_mean_doys_cluster_green.RData")
  # mean_doy_norm <- rbind(mean_doy1, mean_doy2, mean_doy3)
  
  # Possible spring doys
  a_vec <- seq(90,150,by=1)
  # Possible autumn doys (counted backwards from 31.12.)
  b_vec <- seq(30,100,by=1)
  
  # With optimality criterion 1
  ftests <- struc_ftest(mean_doy_norm, uROI=TRUE)
  
  # With optimality criterion 2
  max_corr <- struc_dummy(mean_doy_norm, a_vec, b_vec, doy=NULL)
  
  # check mean_greenness$settings_mat to know which mask corresponds to which setting
  settings_mat[ftests$wm1,]
  settings_mat[ftests$wmx,]
  settings_mat[max_corr$wm1,]
  # settings_mat	
}

sessionInfo()
Sys.time()
