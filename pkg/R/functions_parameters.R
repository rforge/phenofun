doy_mat_func <- function(){
	month_c <- c(rep(1,31),
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
	
	day_c <- c(c(1:31),
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
	
	day0_c <- paste0("0",day_c)
	days_c <- as.character(ifelse(nchar(day_c)==1,day0_c,day_c))
	
	month0_c <- paste0("0",month_c)
	months_c <- as.character(ifelse(nchar(month_c)==1,month0_c,month_c))
	
	doy_mat <- data.frame(md=paste0(months_c,days_c),doy=1:365)
}