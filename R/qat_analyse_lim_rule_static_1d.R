qat_analyse_lim_rule_static_1d <-
function(measurement_vector, min_value = NULL, max_value = NULL) {
## functionality: check measurement_vector for static lim rule
## author: André Düsterhus
## date: 26.11.2009
## version: A0.1
## input: measurement_vector, minimum value, maximum value
## output: list with: vector with flags (0 ok, -1 minimum error, +1 maximum error), minimum value, maximum value
	# initialisation of variables
	if (length(measurement_vector) > 0) {
		flagvector <- array(0.0, length(measurement_vector))
		# check every element of vector
		for (ii in 1:length(measurement_vector)) {
			# just take values which are not NaN (prevent errors in R-routines))
			if (!is.na(measurement_vector[ii])) {
				# check for minimum boundary
				if (!is.null(min_value)) {
					if (measurement_vector[ii] < min_value) {
						flagvector[ii] <- -1
					}
				}
				# check for maximum boundary
				if (!is.null(max_value)) {
					if (measurement_vector[ii] > max_value) {
						flagvector[ii] <- +1
					}
				}
			}
		}
	}
	resultlist<- c(list(flagvector),list(min_value),list(max_value))
	names(resultlist)<-c("flagvector","min_value","max_value") 
	return(resultlist)
}

