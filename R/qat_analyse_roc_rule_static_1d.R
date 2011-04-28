qat_analyse_roc_rule_static_1d <-
function(measurement_vector, max_upward_value = NULL, max_downward_value = NULL) {
## functionality: check measurement_vector for dynamic lim rule
## author: André Düsterhus
## date: 02.12.2009
## version: A0.1
## input: measurement_vector, maximum upward change value, maximum downward change value (both positive)
## output: vector with flags (0 ok, -1 downward error, +1 upward error)
# for tests: versionswitch
version <- 1
	if (version == 1) {
		# initialisation of variables
		flagvector <- array(0.0, length(measurement_vector))
		# check every element of vector
		for (ii in 2:length(measurement_vector)) {
			# just take values which are not NaN (prevent errors in R-routines))
			if (!is.na(measurement_vector[ii]) && !is.na(measurement_vector[ii-1])) {
				# check for upward change boundary
				if(!is.null(max_upward_value)) {
					if ((measurement_vector[ii]-measurement_vector[ii-1]) > max_upward_value) {
						flagvector[ii] <- 1
					}
				}
				if(!is.null(max_downward_value)) {
				# check for maximum boundary
					if ((measurement_vector[ii]-measurement_vector[ii-1]) < (-1.* max_downward_value)) {
						flagvector[ii] <- -1
					}
				}
			}
		}
	} else {
		# initialisation of variables
		flagvector <- array(0.0, length(measurement_vector)-1)
		# check every element of vector
		roc_values<-diff(measurement_vector)
		flagvector[which(roc_values > max_upward_value)] <- 1
		flagvector[which(roc_values > -1.* max_downward_value)] <- -1
	#	print(roc_values)
	}
	resultlist<- c(list(flagvector),list(max_upward_value), list(max_downward_value))
	names(resultlist)<-c("flagvector","max_upward_value", "max_downward_value")
	return(resultlist)
}

