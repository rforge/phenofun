##############################################
# Package phenofun
# Code for reading in the data from foto-webcam.eu for marquartstein sued
# 
# Date: 30.05.16
# Author: Ludwig Bothmann
##############################################

library(EBImage)
	
year <- rep(2013,365)

month <- c(rep(1,31),
			  rep(2,28),
			  rep(3,31),
			  rep(4,30),
			  rep(5,31),
			  rep(6,30),
			  rep(7,31),
			  rep(8,31),
			  rep(9,30),
			  rep(10,31),
			  rep(11,30),
			  rep(12,31))

day <- c(c(1:31),
			  c(1:28),
			  c(1:31),
			  c(1:30),
			  c(1:31),
			  c(1:30),
			  c(1:31),
			  c(1:31),
			  c(1:30),
			  c(1:31),
			  c(1:30),
			  c(1:31))

day0 <- paste0("0",day)
days <- as.character(ifelse(nchar(day)==1,day0,day))

month0 <- paste0("0",month)
months <- as.character(ifelse(nchar(month)==1,month0,month))

#####################
# marquartstein
#####################

# URL of images from Marquartstein
files <- paste0("http://www.foto-webcam.eu/webcam/oed/",year,"/",months,"/",days,"/1200_lm.jpg")

# Folder to save the images in
files_out <- paste0("../../Phenology/data/foto-webcam/marquartstein/",year,"_",months,"_",days,"_1200_lm.jpg")

for(i in c(1:365)){
	cat(i,"\n")
	img <- readImage(files=files[i])
	writeImage(img, files=files_out[i])
}
