# img <- readImage("http://www.foto-webcam.eu/webcam/grandsberg/2012/05/18/1140_lm.jpg")
# display(img)

library(EBImage)
	
year <- rep(2013,365)
year14 <- rep(2014,365)

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
# studentenstadt
#####################

files <- paste0("http://www.foto-webcam.eu/webcam/freimann/",year,"/",months,"/",days,"/1400_lm.jpg")

files_out <- paste0("data/foto-webcam/studentenstadt/",year,"_",months,"_",days,"_1400_lm.jpg")

files[113] <- "http://www.foto-webcam.eu/webcam/freimann/2013/04/23/1420_lm.jpg"
files[353] <- "http://www.foto-webcam.eu/webcam/freimann/2013/12/19/1420_lm.jpg"

files_out[113] <- "data/foto-webcam/studentenstadt/2013_04_23_1420_lm.jpg"
files_out[353] <- "data/foto-webcam/studentenstadt/2013_12_19_1420_lm.jpg"


# writeImage(img, files=files_out[i])
#113#353
for(i in c(1:365)){
	cat(i,"\n")
	img <- readImage(files=files[i])
	writeImage(img, files=files_out[i])
}

#############
# grandsberg
#############

files <- paste0("http://www.foto-webcam.eu/webcam/grandsberg/",year,"/",months,"/",days,"/1200_lm.jpg")

files_out <- paste0("data/foto-webcam/grandsberg/",year,"_",months,"_",days,"_1200_lm.jpg")

files[299] <- "http://www.foto-webcam.eu/webcam/grandsberg/2013/10/26/1210_lm.jpg"
 
files_out[299] <- "data/foto-webcam/grandsberg/2013_10_26_1210_lm.jpg"


# ohne 61 und 62
for(i in c(1:60,63:365)){
	cat(i,"\n")
	img <- readImage(files=files[i])
	writeImage(img, files=files_out[i])
}

#############
# bruneck
#############

files <- paste0("http://www.foto-webcam.eu/webcam/bruneck/",year,"/",months,"/",days,"/1200_lm.jpg")

files_out <- paste0("data/foto-webcam/bruneck/",year,"_",months,"_",days,"_1200_lm.jpg")

files[52] <- "http://www.foto-webcam.eu/webcam/bruneck/2013/02/21/1210_lm.jpg"
files[321] <- "http://www.foto-webcam.eu/webcam/bruneck/2013/11/17/1210_lm.jpg"
files[329] <- "http://www.foto-webcam.eu/webcam/bruneck/2013/11/25/1210_lm.jpg"
files[333] <- "http://www.foto-webcam.eu/webcam/bruneck/2013/11/29/1210_lm.jpg"

files_out[52] <- "data/foto-webcam/bruneck/2013_02_21_1210_lm.jpg"
files_out[321] <- "data/foto-webcam/bruneck/2013_11_17_1210_lm.jpg"
files_out[329] <- "data/foto-webcam/bruneck/2013_11_25_1210_lm.jpg"
files_out[333] <- "data/foto-webcam/bruneck/2013_11_29_1210_lm.jpg"

for(i in c(1:365)){
	cat(i,"\n")
	img <- readImage(files=files[i])
	writeImage(img, files=files_out[i])
}



#####################
# sonnblickbasis
#####################

files <- paste0("http://www.foto-webcam.eu/webcam/sonnblickbasis/",year14,"/",months,"/",days,"/1200_lm.jpg")

files_out <- paste0("data/foto-webcam/sonnblickbasis/",year14,"_",months,"_",days,"_1200_lm.jpg")

files[137] <- "http://www.foto-webcam.eu/webcam/sonnblickbasis/2014/05/17/1300_lm.jpg"
files[148] <- "http://www.foto-webcam.eu/webcam/sonnblickbasis/2014/05/28/1100_lm.jpg"
files[179] <- "http://www.foto-webcam.eu/webcam/sonnblickbasis/2014/06/28/1300_lm.jpg"

files_out[137] <- "data/foto-webcam/sonnblickbasis/2014_05_17_1300_lm.jpg"
files_out[148] <- "data/foto-webcam/sonnblickbasis/2014_05_28_1100_lm.jpg"
files_out[179] <- "data/foto-webcam/sonnblickbasis/2014_06_28_1300_lm.jpg"

# ohne 110,116,150
for(i in c(1:365)[-c(110,116,150)]){
	cat(i,"\n")
	img <- readImage(files=files[i])
	writeImage(img, files=files_out[i])
}

#####################
# marquartstein
#####################

files <- paste0("http://www.foto-webcam.eu/webcam/marquartstein/",year,"/",months,"/",days,"/1200_lm.jpg")

files_out <- paste0("data/foto-webcam/marquartstein/",year,"_",months,"_",days,"_1200_lm.jpg")

for(i in c(1:365)){
	cat(i,"\n")
	img <- readImage(files=files[i])
	writeImage(img, files=files_out[i])
}

#####################
# unterammergau
#####################

files <- paste0("http://www.foto-webcam.eu/webcam/unterammergau/",year,"/",months,"/",days,"/1200_lm.jpg")

files_out <- paste0("data/foto-webcam/unterammergau/",year,"_",months,"_",days,"_1200_lm.jpg")

files[234] <- "http://www.foto-webcam.eu/webcam/unterammergau/2013/08/22/1210_lm.jpg"

files_out[234] <- "data/foto-webcam/unterammergau/2013_08_22_1210_lm.jpg"

for(i in c(1:365)){
	cat(i,"\n")
	img <- readImage(files=files[i])
	writeImage(img, files=files_out[i])
}



#####################
# kranzberg (nicht foto-webcam.eu)
#####################

files <- paste0("data/WebCam/cam7/Kranzberg2_20140501_0945_121.jpg")

files_out <- paste0("data/WebCam/Kranzberg2_20140501_0945_121.jpg")


img <- readImage(files=files)

display(img)

img2 <- imageData(img)[1285:2560,,]

img3 <- Image(img2, colormode = "Color")

display(img3)

writeImage(img3, files=files_out)
