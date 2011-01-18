qat_analyse_lim_rule_dynamic_1d <-
function(measurement_vector, min_vector=NULL, max_vector=NULL, min_vector_name=NULL, max_vector_name=NULL, min_vector_identifier=NULL, max_vector_identifier=NULL) {
## functionality: check measurement_vector for dynamic lim rule
## author: André Düsterhus
## date: 11.04.2011
## version: A0.2
## input: measurement_vector, minimum vector, maximum vector, minimum vector name, maximum vector name, minimum vector identifier,
##        maximum vector identifier
## output: vector with flags (0 ok, -1 minimum error, +1 maximum error)
	# initialisation of variables
	flagvector <- array(0.0, length(measurement_vector))
	# check every element of vector
	if(length(measurement_vector) != length(min_vector)) {
		min_vector <- array(NaN, length(measurement_vector))
	}
	if(length(measurement_vector) != length(max_vector)) {
		max_vector <- array(NaN, length(measurement_vector))
	}
	for (ii in 1:length(measurement_vector)) {
		# just take values which are not NaN (prevent errors in R-routines))
		if (!is.na(measurement_vector[ii])) {
			if (!is.na(min_vector[ii])) {
				# check for minimum boundary
				if (measurement_vector[ii] < min_vector[ii]) {
					flagvector[ii] <- -1
				}
			}
			if (!is.na(max_vector[ii])) {
				# check for maximum boundary
				if (measurement_vector[ii] > max_vector[ii]) {
					flagvector[ii] <- +1
				}
			}
		}
	}
	resultlist<- c(list(flagvector), list(min_vector), list(max_vector), list(min_vector_name), list(max_vector_name), list(min_vector_identifier), list(max_vector_identifier))
	names(resultlist)<-c("flagvector", "min_vector", "max_vector", "min_vector_name", "max_vector_name", "min_vector_identifier", "max_vector_identifier")
	return(resultlist)
}
