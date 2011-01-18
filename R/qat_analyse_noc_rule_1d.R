qat_analyse_noc_rule_1d <-
function(measurement_vector, max_return_elements = NULL) {
## functionality: check measurement_vector for dynamic lim rule
## author: André Düsterhus
## date: 24.01.2010
## version: A0.1
## input: measurement_vector, max_return_elements
## output: vector with flags (0 ok, +1 repetition error)
	# initialisation of variables
	flagvector <- array(0.0, length(measurement_vector))
	if (!is.null(max_return_elements)) {
		# check every element of vector
		for (ii in (max_return_elements+1):length(measurement_vector)) {
			# just take values which are not NaN (prevent errors in R-routines))
			if (!is.na(measurement_vector[ii])) {
				bool = TRUE 
				for (jj in (ii-1):(ii-max_return_elements)) {
					if (!is.na(measurement_vector[jj])) {
						if (measurement_vector[ii] != measurement_vector[jj]) {
							bool = FALSE
						}
					} else {
						bool = FALSE
					}
				}
				if (bool == TRUE) {
					flagvector[ii] = 1
				}
			}
		}
	}
	resultlist<- c(list(flagvector),list(max_return_elements))
	names(resultlist)<-c("flagvector","max_return_elements") 
	return(resultlist)
}
