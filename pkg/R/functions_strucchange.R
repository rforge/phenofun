###########################################################################
# Functions for tests for structural changes
# Date: 18.01.16
# Author: Ludwig Bothmann
###########################################################################


#' Function to apply optimality criterion 1 on \%greenness time series
#' 
#' @param mean_doy_norm matrix with \%greenness time series in rows
#' @param cut240 Cut at DOY 240? Default is \code{TRUE}
#' @param x_vec Optionaly vector of DOYs
#' @param uROI \code{TRUE}: uROI, \code{FALSE}: sROI. Default is \code{TRUE}
#' @param settings MAtrix with settings, only for sROI.
#' @return max_f Maximal f statistics
#' @export
#' @import strucchange
struc_ftest <- function(mean_doy_norm, 
								cut240=TRUE,
								x_vec=NULL,
								uROI=TRUE,
								settings){
	
	if(is.null(x_vec)){
		# Tage des Jahres
		x_vec <- 1:ncol(mean_doy_norm)
	}
	
	if(cut240){
		if(ncol(mean_doy_norm)>=240){
			x_vec <- 1:240
			mean_doy_norm <- mean_doy_norm[,1:240]
		}else{
			x_vec <- 1:ncol(mean_doy_norm)
			mean_doy_norm <- mean_doy_norm[,1:ncol(mean_doy_norm)]
		}
	}
	
	# Plot der Gruenzeitreihen
	plot(x_vec, mean_doy_norm[1,], type="l")#, ylim=c(0.3,.45))
	for(i in 2:nrow(mean_doy_norm)){
		lines(x_vec, mean_doy_norm[i,], col=i, lty=i)
	}
	
	# Maximale F-Statistiken initialisieren
	max_f <- matrix(nrow=nrow(mean_doy_norm), ncol=2)
	colnames(max_f) <- c("constant fit", "linear fit")
	
	# Bruchpunkte initialisieren
	bps <- matrix(nrow=nrow(mean_doy_norm), ncol=2)
	
	# Alle Zeitreihen durchgehen
	for(i in 1:nrow(mean_doy_norm)){
		
		if(!any(is.nan(mean_doy_norm[i,]))){
			
			# Strukturbruch finden konstantes Modell
			fs1 <- Fstats(mean_doy_norm[i,]~1) #  oder ~1 # +I(x_vec^2)
			
			# Maximale F-Statistik abspeichern
			max_f[i,1] <- max(fs1$Fstats)
			
			# Strukturbruch abspeichern
			bps[i,1] <- breakpoints(fs1)$breakpoints
			
			# Strukturbruch finden lineares Modell
			fsx <- Fstats(mean_doy_norm[i,]~x_vec) #  oder ~1
			
			# Maximale F-Statistik abspeichern
			max_f[i,2] <- max(fsx$Fstats)
			
			# Strukturbruch abspeichern
			bps[i,2] <- breakpoints(fsx)$breakpoints
			
		}
	}
	
	# Bruchpunkte der Zeitreihen
	bps
	
	# Maximale F-Statistik in den Zeitreihen
	max_f
	round(max_f)
	
	# Nach Groesse sortieren
	ranking <- order(max_f[,2],decreasing = TRUE)
	max_f_order <- max_f[ranking,]
	head(max_f_order)
	
	if(uROI){
		
		# Maske mit der hoechsten F-Statistik
		wm1 <- which.max(max_f[,1])
		cat("Mask", wm1,"is the best for constant fit. Breakpoint at DOY",bps[wm1,1],"\n")
		cat("F-Test statistic:",max_f[wm1,1],"\n")
		wmx <- which.max(max_f[,2])
		cat("Mask", wmx, "is the best for linear fit. Breakpoint at DOY",bps[wmx,2]," \n")
		cat("F-Test statistic:",max_f[wmx,2],"\n")
	}
	
	if(!uROI){
		
		# Maske mit der hoechsten F-Statistik
		wm1 <- which.max(max_f[,1])
		cat("Maske", wm1, "mit Region", settings[wm1,2],
			 " und Threshold", settings[wm1,1], 
			 "ist die beste bei konstantem Fit! \n")
		cat("F-Test-Statistik:",max_f[wm1,1],"\n")
		wmx <- which.max(max_f[,2])
		cat("Maske", wmx, "mit Region", settings[wmx,2],
			 " und Threshold", settings[wmx,1], 
			 "ist die beste bei linearem Fit! \n")
		cat("F-Test-Statistik:",max_f[wmx,2],"\n")
	}
	
	# Plot der Gruenzeitreihen mit den Strukturbruechen
	plot(x_vec, mean_doy_norm[1,], type="l")#, ylim=c(0.3,.45))
	for(i in 2:nrow(mean_doy_norm)){
		lines(x_vec, mean_doy_norm[i,], col=i, lty=i)
	}
	abline(v=bps, col=1:nrow(mean_doy_norm))
	
	plot(x_vec, mean_doy_norm[ranking[1],], type="l")#, ylim=c(0.3,.45))
	# abline(v=bps[1,1])
	for(i in 2:5){
		
		lines(x_vec, mean_doy_norm[ranking[i],], col=i, lty=i)
		# abline(v=bps[i,1])
	}
	
	return(list(max_f=max_f,
					wm1=wm1,
					wmx=wmx))
}



#' Help function for computation of dummy time series
#' 
#' @param x Vector of time points
#' @param a First break point at a
#' @param b Second break point at 365-b
#' @param doy Vector of doys
#' 
dummy_time <- function(x,a,b, doy=NULL){
	
	xmax <- max(x)
	# Only for combinations of a and b such that dummy time series possible
	if(a+15+55+15+b<=xmax){
		# 		y <- c(rep(1,a), seq(from = 1, to = 4,length= 10), seq(from=4, to=3, length=55),
		# 				 rep(3,130), seq(from=3, to=1, length=10), rep(1, xmax-a-205))
		y <- c(rep(.34,a), seq(from = .34, to = .42,length= 15), seq(from=.42, to=.39, length=55),
				 rep(.39,max(xmax-a-b-85,0)), seq(from=.39, to=.34, length=15), rep(.34, b))
		if(!is.null(doy)){
			y <- y[doy]
		}
	}else{
		# y <- rep(0,xmax)
		stop("a = ", a," and b = ",b," in sum to large to compute dummy time series of length ", xmax, ".")
	}
	return(y)
}

#' Function to apply optimality criterion 2 on \%greenness time series
#' 
#' @param mean_doy_norm matrix with \%greenness time series in rows
#' @param a_vec Vector of possible spring doys
#' @param b_vec Vector of possible autumn doys (backwards from 31.12.)
#' @param doy Optional vector of DOYs
#' @return max_corr Maximal correlations
#' @export
struc_dummy <- function(mean_doy_norm,
								a_vec,
								b_vec,
								doy=NULL
){
	
	if(is.null(doy)){
		# Tage des Jahres
		x_vec <- 1:ncol(mean_doy_norm)
	}else{
		x_vec <- doy
	}
	
	xmax <- max(x_vec)
	
	# Maximale Korrelation initialisieren
	max_corr <- vector(length=nrow(mean_doy_norm))
	
	# Matrix mit allen Korrelationen initialisieren
	cor_array <- array(0,dim = c(nrow(mean_doy_norm), length(a_vec), length(b_vec)))
	
	# Alle Zeitreihen durchgehen
	for(i in 1:nrow(mean_doy_norm)){
		
		# Wenn keine NAs drin sind
		if(!any(is.nan(mean_doy_norm[i,]))){
			
			# Alle Startpunkte des Fruehlings durchgehen
			for(j in seq_len(length(a_vec))){
				
				# Alle Laengen Winter ab Herbstende durchgehen
				for(b in seq_len(length(b_vec))){
					
					# Only for combinations of a and b such that dummy time series is possible
					if(a_vec[j]+15+55+15+b_vec[b]<=xmax){
						
						# Dummyzeitreihe erstellen
						y <- dummy_time(x_vec,a_vec[j],b_vec[b],doy=doy)
						
						# Korrelation ausrechnen
						cor_array[i,j,b] <- cor(y, mean_doy_norm[i,])
						
					}else{
						cor_array[i,j,b] <- 0
					}
				}
			}
			
			# Hoechste Korrelation pro Farbzeitreihe abspeichern
			max_corr[i] <- max(cor_array[i,,])
			
		}
	}
	
	# Korrelationen
	round(max_corr,2)
	
	# Nach Groesse sortieren
	ranking <- order(max_corr,decreasing = TRUE)
	max_corr_order <- max_corr[ranking]
	
	# Maske mit der hoechsten Korrelation
	wm1 <- which.max(max_corr)
	cat("Mask", wm1,"is the best regarding optimality criterion 2 (dummy time series)! \n")
	cat("Correlation:",max_corr[wm1],"\n")
	
	# Welcher Kombination von a und b ist am besten? (index)
	ind <- which.max(cor_array[wm1,,])
	ind_opt <- arrayInd(ind, .dim = dim(cor_array)[2:3])
	
	a_opt <- a_vec[ind_opt[1]]
	b_opt <- b_vec[ind_opt[2]]
	
	cat("Best start of spring: DOY", a_opt," \n")
	cat("Best start of autumn: DOY", max(x_vec)-b_opt," \n")
	
	return(list(max_corr=max_corr,
					wm1=wm1))
	
}
