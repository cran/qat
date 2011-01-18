qat_analyse_roc_rule_dynamic_1d <-
function(measurement_vector, max_upward_vector=NULL, max_downward_vector=NULL, upward_vector_name=NULL, downward_vector_name=NULL, upward_vector_identifier=NULL, downward_vector_identifier=NULL) {
## functionality: check measurement_vector for dynamic lim rule
## author: André Düsterhus
## date: 02.12.2009
## version: A0.1
## input: measurement_vector, maximum upward change vector, maximum downward change vector (both positive), maximum upward change vector name, 
##        maximum downward change vector name, maximum upward change vector identifier, maximum downward change vector identifier
## output: vector with flags (0 ok, -1 downward error, +1 upward error)
	# initialisation of variables
	flagvector <- array(0.0, length(measurement_vector))
	if(length(measurement_vector) != length(max_downward_vector)) {
		max_downward_vector <- array(NaN, length(measurement_vector))
	}
	if(length(measurement_vector) != length(max_upward_vector)) {
		max_upward_vector <- array(NaN, length(measurement_vector))
	}
	# check every element of vector
	for (ii in 2:length(measurement_vector)) {
		# just take values which are not NaN (prevent errors in R-routines))
		if (!is.na(measurement_vector[ii]) && !is.na(measurement_vector[ii-1])) {
			# check for upward change boundary
			if (!is.na(max_upward_vector[ii])) {
				if ((measurement_vector[ii]-measurement_vector[ii-1]) > max_upward_vector[ii]) {
					flagvector[ii] <- 1
				}
			}
			# check for maximum boundary
			if (!is.na(max_downward_vector[ii])) {
				if ((measurement_vector[ii]-measurement_vector[ii-1]) < (-1.* max_downward_vector[ii])) {
					flagvector[ii] <- -1
				}
			}
		}
	}
	resultlist<- c(list(flagvector), list(max_upward_vector), list(max_downward_vector), list(upward_vector_name), list(downward_vector_name), list(upward_vector_identifier), list(downward_vector_identifier))
	names(resultlist)<-c("flagvector", "max_upward_vector", "max_downward_vector", "upward_vector_name", "downward_vector_name", "upward_vector_identifier", "downward_vector_identifier")
	return(resultlist)
}
