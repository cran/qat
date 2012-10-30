qat_analyse_roc_rule_static_2d <-
function(measurement_vector, max_upward_value = NULL, max_downward_value = NULL) {
## functionality: check measurement_vector for dynamic lim rule
## author: André Düsterhus
## date: 30.07.2011
## version: A0.1
## input: measurement_vector, maximum upward change value, maximum downward change value (both positive)
## output: vector with flags (0 ok, -1 downward error, +1 upward error)
	# initialisation of variables
	flagvector <- array(0.0, c(dim(measurement_vector)[1], dim(measurement_vector)[2]))
	# check every element of vector
	for (ii in 2:dim(measurement_vector)[1]) {
		for (jj in 1:dim(measurement_vector)[2]) {
			# just take values which are not NaN (prevent errors in R-routines))
			if (!is.na(measurement_vector[ii, jj]) && !is.na(measurement_vector[ii-1, jj])) {
				# check for upward change boundary
				if(!is.null(max_upward_value)) {
					if ((measurement_vector[ii, jj]-measurement_vector[ii-1, jj]) > max_upward_value) {
						flagvector[ii, jj] <- 1
					}
				}
				if(!is.null(max_downward_value)) {
				# check for maximum boundary
					if ((measurement_vector[ii, jj]-measurement_vector[ii-1, jj]) < (-1.* max_downward_value)) {
						flagvector[ii, jj] <- -1
					}
				}
			}
		}
	}
	resultlist<- c(list(flagvector),list(max_upward_value), list(max_downward_value))
	names(resultlist)<-c("flagvector","max_upward_value", "max_downward_value")
	return(resultlist)
}
