qat_analyse_lim_rule_sigma_1d <-
function(measurement_vector, sigma_factor=NULL) {
## functionality: check measurement_vector for static lim rule
## author: André Düsterhus
## date: 15.01.2010
## version: A0.1
## input: measurement_vector, sigma_factor
## output: vector with flags (0 ok, -1 minimum error, +1 maximum error)
	# initialisation of variables
	if ((length(measurement_vector) > 0) && !is.null(sigma_factor)) {
		flagvector <- array(0.0, length(measurement_vector))
		meanofvector<-mean(measurement_vector, na.rm=TRUE)
		sdofvector<-sd(measurement_vector, na.rm=TRUE)
		# check every element of vector
		for (ii in 1:length(measurement_vector)) {
			# just take values which are not NaN (prevent errors in R-routines))
			if (!is.na(measurement_vector[ii])) {
				# check for minimum boundary
				if (measurement_vector[ii] < meanofvector-(sigma_factor*sdofvector)) {
					flagvector[ii] <- -1
				}
				# check for maximum boundary
				if (measurement_vector[ii] > meanofvector+(sigma_factor*sdofvector)) {
					flagvector[ii] <- +1
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
