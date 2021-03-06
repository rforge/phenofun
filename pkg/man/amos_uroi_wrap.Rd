% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions_amos.R
\name{amos_uroi_wrap}
\alias{amos_uroi_wrap}
\title{Wrapper function for analyzing data from AMOS}
\usage{
amos_uroi_wrap(camera, year_analysis, months_analysis, hour_analysis,
  do_mean_greenness = TRUE, do_strucchange = TRUE, testmode = FALSE,
  only_download = FALSE, folder_results = getwd(),
  name_of_analysis = substr(as.character(Sys.time()), 1, 10),
  folder_data = getwd(), n_pc_vec = 12, k_vec = 4:10, nstart = 2,
  save_results = TRUE, save_masks = TRUE, masks_type = ".jpg",
  a_vec = seq(1, 150, by = 1), b_vec = seq(1, 100, by = 1), mask = NULL,
  ...)
}
\arguments{
\item{camera}{Name of the camera (five-digit number, see \url{http://amos.cse.wustl.edu/})}

\item{year_analysis}{Year(s) to be analyzed as vector}

\item{months_analysis}{Month(s) to be analyzed as vector}

\item{hour_analysis}{Hour(s) to be analyzed as vector}

\item{do_mean_greenness}{If \code{TRUE} (default), time series of percentage 
greenness inside the ROIs are computed}

\item{do_strucchange}{If \code{TRUE} (default), points of structural changes 
are searched for and OC values are computed}

\item{testmode}{If \code{TRUE}, only 10 images for testing are analyzed, 
default is \code{FALSE}}

\item{only_download}{If \code{TRUE}, images are downloaded but not analyzed,
default is \code{FALSE}}

\item{folder_results}{Folder where the results will be saved, default is
the current working directory}

\item{name_of_analysis}{Name of subfolder for the results of the analysis,
default is the date as \code{yyyy-mm-dd}}

\item{folder_data}{Folder where the data will be saved, default is 
the current working directory}

\item{n_pc_vec}{Vector of numbers of eigenimages}

\item{k_vec}{Vector of numbers of clusters for k-means}

\item{nstart}{Number of iterations k-means}

\item{save_results}{If \code{TRUE} (default), results of clustering are saved}

\item{save_masks}{If \code{TRUE} (default), masks are saved}

\item{masks_type}{File name extension of masks, default is \code{.jpg}}

\item{a_vec}{Possible spring doys}

\item{b_vec}{Possible autumn doys (counted backwards from 31.12.)}

\item{mask}{Do not edit, masks will be loaded automatically}

\item{...}{Further arguments}
}
\value{
As output, all resulting masks and percentage greenness time series 
 inside the masks are saved in subfolders of \code{folder_results}. 
 Additionally, overlays of all masks of the best setting with a background 
 image and the background image itself are saved in \code{folder_results}, 
 ordered with respect to optimality criterion OC2.
}
\description{
This function allows to analyze webcam data from AMOS 
(\url{http://amos.cse.wustl.edu/}) with the uROI method in a fully 
automated way. Only the parameters of camera, year, month and hour of images to be
analyzed have to be specified. Example code can be downloaded at 
\url{http://bothmann.userweb.mwn.de/dissertation.html}.
}

