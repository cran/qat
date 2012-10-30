qat_analyse_roc_rule_dynamic_2d <-
function(measurement_vector, max_upward_vector=NULL, max_downward_vector=NULL, upward_vector_name=NULL, downward_vector_name=NULL, upward_vector_identifier=NULL, downward_vector_identifier=NULL) {
## functionality: check measurement_vector for dynamic lim rule
## author: André Düsterhus
## date: 30.07.2011
## version: A0.1
## input: measurement_vector, maximum upward change vector, maximum downward change vector (both positive), maximum upward change vector name, 
##        maximum downward change vector name, maximum upward change vector identifier, maximum downward change vector identifier
## output: vector with flags (0 ok, -1 downward error, +1 upward error)
	# initialisation of variables
	flagvector <- array(0.0, c(dim(measurement_vector)[1], dim(measurement_vector)[2]))
	if (!is.null(dim(max_downward_vector))) {
		if((dim(measurement_vector)[1] != dim(max_downward_vector)[1]) || (dim(measurement_vector)[2] != dim(max_downward_vector)[2])) {
			max_downward_vector <- array(NaN, c(dim(measurement_vector)[1], dim(measurement_vector)[2]))
		}
	} else {
		max_downward_vector <- array(NaN, c(dim(measurement_vector)[1], dim(measurement_vector)[2]))
	}
	if (!is.null(dim(max_upward_vector))) {
		if((dim(measurement_vector)[1] != dim(max_upward_vector)[1]) || (dim(measurement_vector)[2] != dim(max_upward_vector)[2])) {
			max_upward_vector <- array(NaN, c(dim(measurement_vector)[1], dim(measurement_vector)[2]))
		}
	} else {
		max_upward_vector <- array(NaN, c(dim(measurement_vector)[1], dim(measurement_vector)[2]))
	}
	# check every element of vector
	for (ii in 2:dim(measurement_vector)[1]) {
		for (jj in 2:dim(measurement_vector)[2]) {
			# just take values which are not NaN (prevent errors in R-routines))
			if (!is.na(measurement_vector[ii, jj]) && !is.na(measurement_vector[ii-1, jj])) {
				# check for upward change boundary
				if (!is.na(max_upward_vector[ii, jj])) {
					if ((measurement_vector[ii, jj]-measurement_vector[ii-1, jj]) > max_upward_vector[ii, jj]) {
						flagvector[ii, jj] <- 1
					}
				}
				# check for maximum boundary
				if (!is.na(max_downward_vector[ii, jj])) {
					if ((measurement_vector[ii, jj]-measurement_vector[ii-1, jj]) < (-1.* max_downward_vector[ii, jj])) {
						flagvector[ii, jj] <- -1
					}
				}
			}
		}
	}
	resultlist<- c(list(flagvector), list(max_upward_vector), list(max_downward_vector), list(upward_vector_name), list(downward_vector_name), list(upward_vector_identifier), list(downward_vector_identifier))
	names(resultlist)<-c("flagvector", "max_upward_vector", "max_downward_vector", "upward_vector_name", "downward_vector_name", "upward_vector_identifier", "downward_vector_identifier")
	return(resultlist)
}
