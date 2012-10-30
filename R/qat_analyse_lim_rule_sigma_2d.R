qat_analyse_lim_rule_sigma_2d <-
function(measurement_vector, sigma_factor=NULL) {
## functionality: check measurement_vector for static lim rule
## author: André Düsterhus
## date: 30.07.2011
## version: A0.1
## input: measurement_vector, sigma_factor
## output: vector with flags (0 ok, -1 minimum error, +1 maximum error)
	# initialisation of variables
	if ((dim(measurement_vector)[1] > 0) && !is.null(sigma_factor)) {
		flagvector <- array(0.0, c(dim(measurement_vector)[1], dim(measurement_vector)[2]))
		meanofvector<-mean(measurement_vector, na.rm=TRUE)
		sdofvector<-sd(as.vector(measurement_vector), na.rm=TRUE)
		# check every element of vector
		for (ii in 1:dim(measurement_vector)[1]) {
			for (jj in 1:dim(measurement_vector)[2]) {
				# just take values which are not NaN (prevent errors in R-routines))
				if (!is.na(measurement_vector[ii, jj])) {
					# check for minimum boundary
					if (measurement_vector[ii, jj] < meanofvector-(sigma_factor*sdofvector)) {
						flagvector[ii, jj] <- -1
					}
					# check for maximum boundary
					if (measurement_vector[ii, jj] > meanofvector+(sigma_factor*sdofvector)) {
						flagvector[ii, jj] <- +1
					}
				}
			}
		}
	} else {
		flagvector <- NULL
		meanofvector <- NULL
		sdofvector <- NULL
	}
	resultlist<- c(list(flagvector), list(sigma_factor), list(meanofvector), list(sdofvector))
	names(resultlist)<-c("flagvector","sigma_factor", "meanofvector", "sdofvector") 
	return(resultlist)
}
