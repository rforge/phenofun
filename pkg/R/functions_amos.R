###########################################################################
# Functions for downloading data from AMOS
# Date: 15.06.16
# Author: Ludwig Bothmann
###########################################################################

###################################
# download_amos_func()
###################################

#' Function to download image data from AMOS
#' 
#' @param camera Name(s) of the camera (is a number, see URL) as vector
#' @param year Year(s) to be downloaded as vector
#' @param month Month(s) to be downloaded as vector
#' @export
download_amos_func <- function(camera,
                               year,
                               month){
  
  python_string1 <- "# download_amos.py
# Austin Abrams, 2/16/10
# a helper utility to download and unzip a lot of images from the AMOS dataset.

import os
import sys
import urllib2
import StringIO
import zipfile
import threading
import time
# Change this to where you want data to be dumped off.  If not supplied, defaults to
# the current working directory.
# example:
# ROOT_LOCATION = '/path/to/where/you/want/AMOS_Data/'
ROOT_LOCATION = None

# change these parameters as necessary to download whichever camera or year or month you
# want to download.
CAMERAS_TO_DOWNLOAD = ["
  
  python_string2 <- "]
YEARS_TO_DOWNLOAD = ["
  
  
  python_string3 <- "]
#MONTHS_TO_DOWNLOAD = range(1,13)
MONTHS_TO_DOWNLOAD = ["
  
  python_string4 <- "]
# if the script crashed or the power went out or something, this flag will
# skip downloading and unzipping a month's worth of images if there's already
# a folder where it should be.  If you set this to false, then downloads
# will overwrite any existing files in case of filename conflict.
SKIP_ALREADY_DOWNLOADED = True

# maximum number of threads allowed. This can be changed.
MAX_THREADS = 100

class DownloadThread(threading.Thread):
	camera_id = None
	year = None
	month = None

	def __init__(self, camera_id, year, month):
		threading.Thread.__init__(self)

		self.camera_id = camera_id
		self.year = year
		self.month = month

	def run(self):
		location = ROOT_LOCATION + '%08d/%04d.%02d/' % (self.camera_id, self.year, self.month)

		if SKIP_ALREADY_DOWNLOADED and os.path.exists(location):
			print(location + ' already downloaded.')
			return

		print('downloading to ' + location)
		zf = download(self.camera_id, self.month, self.year)
		print('completed downloading to ' + location)

		if not zf:
			print('skipping ' + location)
			return

		ensure_directory_exists(location)

		print('Extracting from ' + location)
		extract(zf, location)
		print('Done')


def download(camera_id, month, year):

# Downloads a zip file from AMOS, returns a file.

	last_two_digits = camera_id % 100;
	last_four_digits = camera_id % 10000;

	if year < 2013 or year == 2013 and month < 9:
		ZIPFILE_URL = 'http://amosweb.cse.wustl.edu/2012zipfiles/'
	else :
		ZIPFILE_URL = 'http://amosweb.cse.wustl.edu/zipfiles/'
	url = ZIPFILE_URL + '%04d/%02d/%04d/%08d/%04d.%02d.zip' % (year, last_two_digits, last_four_digits, camera_id, year, month)
	#print '    downloading...',
	sys.stdout.flush()

	try:
		result = urllib2.urlopen(url)
	except urllib2.HTTPError as e:
		print e.code, 'error.'
		return None

	handle = StringIO.StringIO(result.read())

	#print 'done.'
	sys.stdout.flush()

	return handle

def extract(file_obj, location):

	#Extracts a bunch of images from a zip file.

	#print '    extracting zip...',
	sys.stdout.flush()

	zf = zipfile.ZipFile(file_obj, 'r')
	zf.extractall(location)
	zf.close()
	file_obj.close()

	#print 'done.'
	sys.stdout.flush()

def ensure_directory_exists(path):

	#Makes a directory, if it doesn't already exist.

	dir_path = path.rstrip('/')       

	if not os.path.exists(dir_path):
		parent_dir_path = os.path.dirname(dir_path)
		ensure_directory_exists(parent_dir_path)

		try:
			os.mkdir(dir_path)
		except OSError:
			pass


def main():
	# for all cameras...
	for camera_id in CAMERAS_TO_DOWNLOAD:
		# for all years...
		for year in YEARS_TO_DOWNLOAD:
			# for all months of imagery...
			for month in MONTHS_TO_DOWNLOAD:

					thread_count = threading.activeCount()
					while thread_count > MAX_THREADS:
						print('Waiting for threads to finish...')
						time.sleep(1)
						thread_count = threading.activeCount()              

					download_thread = DownloadThread(camera_id=camera_id, year=year, month=month)
					download_thread.start()


if __name__ == '__main__':

	if ROOT_LOCATION == None:
		ROOT_LOCATION = os.getcwd() + '/AMOS_Data'

	if ROOT_LOCATION[-1] != '/':
		ROOT_LOCATION = ROOT_LOCATION + '/'
	print 'Downloading images to:'

	main()"
  
  camera_paste <- paste(camera,collapse=",")
  year_paste <- paste(year,collapse=",")
  month_paste <- paste(month,collapse=",")
  
  python_string <- paste0(python_string1,camera_paste,
                          python_string2,year_paste,
                          python_string3,month_paste,
                          python_string4)
  
  write.table(python_string, "download.txt", quote=FALSE, 
              row.names = FALSE, col.names = FALSE)
  
  system("python download.txt")
}

###################################
# amos_uroi_wrap()
###################################

#' Wrapper function for analyzing data from AMOS (http://amos.cse.wustl.edu/)
#' 
#' @param camera Name of the camera (is a number, see URL)
#' @param year_analysis Year(s) to be analyzed as vector
#' @param months_analysis Month(s) to be analyzed as vector
#' @param hour_analysis Hour(s) to be analyzed as vector
#' @param do_mean_greenness If \code{TRUE} (default), time series of percentage 
#'  greenness inside the ROIs are computed
#' @param do_strucchange If \code{TRUE} (default), points of structural change 
#'  are searched for and OC values computed
#' @param testmode If \code{TRUE}, only 10 images for testing are analyzed, 
#'  default is \code{FALSE}
#' @param only_download If \code{TRUE}, images are downloaded but not analyzed,
#'  default is \code{FALSE}
#' @param folder_results Base folder where the results will be saved, default is wd
#' @param name_of_analysis Name of subfolder for the results of this run,
#'  default is the date as \code{yyyy-mm-dd}
#' @param folder_data Folder where the data will be saved, default is wd
#' @param n_pc_vec Vector of numbers of eigenimages
#' @param k_vec Vector of numbers of clusters for k-means
#' @param n_start Number of iterations k-means
#' @param save_results \code{TRUE} if results of clustering shall be saved
#' @param save_masks \code{TRUE} if masks shall be saved
#' @param masks_type File name extension of masks, default is \code{.jpg}
#' @param a_vec Possible spring doys
#' @param b_vec Possible autumn doys (counted backwards from 31.12.)
#' @export
amos_uroi_wrap <- function(camera,
                           year_analysis,
                           months_analysis,
                           hour_analysis,
                           do_mean_greenness=TRUE,
                           do_strucchange=TRUE,
                           testmode=FALSE,
                           only_download=FALSE,
                           folder_results=getwd(),
                           name_of_analysis=substr(as.character(Sys.time()),1,10),
                           folder_data=getwd(),
                           n_pc_vec=12,
                           k_vec=4:10,
                           nstart=2,
                           save_results=TRUE,
                           save_masks=TRUE,
                           masks_type=".jpg",
                           a_vec = seq(90,150,by=1),
                           b_vec = seq(30,100,by=1),
                           ...){
  
  set.seed(1112)
  
  # Transform number of camera in 5-digit string
  camera_ch <- as.character(camera)
  camera_5digit <- substr(paste0("0000",camera_ch),nchar(camera_ch),5+nchar(camera_ch))
  
  # Base directory for the results
  path_base <- paste0(folder_results,"/000",camera_5digit,"/")
  
  # Directory containing the images
  folder <- paste0(folder_data,"/AMOS_Data/000",camera_5digit,"/")
  
  if(testmode){
    
    n_images_try <- 10 
    
    # Parameters for the clusteranalysis
    n_pc_vec <- 2
    k_vec <- 2:3
    
    # Iterations of kmeans
    nstart <- 1
    do_strucchange <- FALSE
  }
  
  ###########################################
  # Download data
  ###########################################
  
  # Set working directory to path where to save the data
  current_wd <- getwd()
  setwd(folder_data)
  
  # Download amos data
  download_amos_func(camera=camera, year=year_analysis, month=months_analysis)
  
  # Set working directory back to initial wd
  setwd(current_wd)
  
  if(!only_download){
    
    ###########################################
    # Extract names of images
    ###########################################
    
    # Get vector with file names of relevant images
    months <- 1:12
    month0 <- paste0("0",months)
    months <- as.character(ifelse(nchar(months)==1,month0,months))
    
    list_files <- vector("list",length = length(months_analysis))
    for(i in months_analysis){
      list_files[[i]] <- list.files(path=paste0(folder,year_analysis,".",months[i],"/"))
    }
    
    lists_files0 <- unlist(list_files)
    month <- substr(lists_files0,5,6)
    lists_files <- paste0(year_analysis,".",month,"/",lists_files0)
    
    # Extract year and doy from file name...
    year <- as.numeric(substr(lists_files0,1,4))
    monthday <- data.frame(md=substr(lists_files0,5,8))
    
    doy_mat <- doy_mat_func()
    md_merge <- merge(monthday, doy_mat, by.x="md", by.y="md",all.x=TRUE)
    doy <- md_merge$doy
    
    # Extract time from file name
    hour <- as.numeric(substr(lists_files0,10,11))
    
    # Title for plot
    main_plot <- paste0("Camera: ",camera_5digit,", Year: ",year_analysis)
    
    # Select images of year "year_analysis" and hour "hour_analysis"
    which_images <- seq_len(length(lists_files0))[which(year%in%year_analysis & 
                                                          hour%in%hour_analysis)]
    
    if(testmode){
      
      # Only first n_images_try images in the folder
      which_images <- which_images[seq_len(n_images_try)]
    }
    
    # Save parameter list for further reference
    parameters_list <- list(camera=camera,
                            year_analysis=year_analysis,
                            months_analysis=months_analysis,
                            hour_analysis=hour_analysis,
                            path_base=path_base, 
                            folder=folder,
                            n_pc_vec=n_pc_vec,
                            k_vec=k_vec,
                            nstart=nstart,
                            testmode=testmode,
                            n_img=length(which_images))
    # lists_files=lists_files
    
    print(parameters_list)
    
    # create directories if not done manually
    create_dirs_uroi(path_base=path_base,
                     name_of_analysis=name_of_analysis)
    
    save(parameters_list,file = paste0(path_base, name_of_analysis,"/parameters.RData"))
    
    
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
                                  save_results=save_results,
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
                                 save_masks=save_masks,
                                 type=masks_type)
    
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
    
    if(do_strucchange){
      
      # take \%greenness from step 3
      mean_doy_norm <- mean_greenness$mean_doy_norm
      settings_mat <- mean_greenness$settings_mat
      
      # # or load it
      # load(paste0(path_base,name_of_analysis,"/mask_timeseries/mean_doys_uROI_green.RData"))
      
      # With optimality criterion 1
      ftests <- struc_ftest(mean_doy_norm, uROI=TRUE)
      
      # With optimality criterion 2
      max_corr <- struc_dummy(mean_doy_norm, a_vec, b_vec, doy=NULL)
      
      # check mean_greenness$settings_mat to know which mask corresponds to which setting
      settings_mat[ftests$wm1,]
      settings_mat[ftests$wmx,]
      settings_mat[max_corr$wm1,]
      # settings_mat	
      
      settings_oc1_oc2 <- data.frame(settings_mat, ftests$max_f, oc2=max_corr$max_corr)
      
      save(settings_oc1_oc2, file = paste0(path_base, name_of_analysis,
                                           "/settings_oc1_oc2.RData"))
      
      #################################################################
      #	5. Plot overlay of best mask
      #################################################################
      
      # Read background image
      img_color <- readImage(paste0(folder,lists_files[which_images][round(length(which_images)/2)]))
      
      #     images <- read_images(colormode="Color",
      #                           folder=folder,
      #                           file_names=lists_files[which_images][round(length(which_images)/2)],
      #                           which_images=1,
      #                           x=NULL,
      #                           y=NULL,
      #                           col_out=1:3,
      #                           col_in=1:3,
      #                           sum1=FALSE,
      #                           norm_it=FALSE,
      #                           colorspace="rgb")
      #     # As color image
      #     img_color <- Image(images,colormode = "Color")
      #     # display(img_color)
      
      # Save background image
      writeImage(img_color,files=paste0(path_base,name_of_analysis,"/background_image.jpg"))
      
      # Find optimal setting
      # load("results/amos/00441/160615_hour15/settings_oc1_oc2.RData")
      K_opt <-	settings_mat[max_corr$wm1,1] 
      npc_opt <-	settings_mat[max_corr$wm1,3]
      opt_setting <- settings_oc1_oc2[which(settings_oc1_oc2[,1]==K_opt & 
                                              settings_oc1_oc2[,3]==npc_opt),]
      o <- order(opt_setting[,6], decreasing = TRUE)
      
      # Plot all masks of optimal setting
      for(i in 1:length(o)){
        load(paste0(path_base,name_of_analysis,"/masks/npc",npc_opt,"_K",
                    K_opt,"_",o[i],".RData"))
        # o[i]
        imgn2 <- img_color/1.5
        imgn2[,,2][which(mask==1)] <- 1
        imgn2[,,1][which(mask==1)] <- 1
        imgn2[,,3][which(mask==1)] <- 0
        imgn2[5:75,5:75,] <- 1
        writeImage(imgn2,files=paste0(path_base,name_of_analysis,
                                      "/uroi_sort_",i,".jpg"))
      }
    }
  }
  
  cat(as.character(Sys.time()),": camera =",camera, "done \n")
  cat("============================================ \n \n")
  
}