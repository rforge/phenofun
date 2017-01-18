###########################################################################
# Functions for plotting
# Date: 18.01.16
# Author: Ludwig Bothmann
###########################################################################

############################################################
# Funktion um Farbzeitreihe und LOESS-Schaetzer zu plotten
############################################################

#' @import graphics
plot_fun <- function(mean_doy, 
							x=1:365,
							main=NULL,
							f = .03,
							ylim=c(.33, .42),
							ylab="Percentage Green",
							cex.lab=1.5,
							cex.main=1.5,
							cex.axis=1.5){
	plot(x, mean_doy, type="l", 
		  main=main,
		  xlab="DOY", 
		  ylim=ylim,
		  ylab=ylab,
		  axes=FALSE,
		  col=rgb(0,0,0,.5),
		  cex.lab=cex.lab,
		  cex.main=cex.main)
	axis(side = 1, at = (c(1,seq(20,360,by=20),365)),#[seq(6,616,by=10)], 
		  labels = c(1,seq(20,360,by=20),365),
		  cex.axis=cex.axis)#[seq(6,616,by=10)])
	axis(side = 2, cex.axis=cex.axis)
	lines(lowess(x,mean_doy,f = f),
			col="blue", lwd=2, lty=2)
}