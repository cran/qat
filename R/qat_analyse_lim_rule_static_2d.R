qat_analyse_lim_rule_static_2d <-
function(measurement_vector, min_value = NULL, max_value = NULL) {
## functionality: check measurement_vector for static lim rule
## author: André Düsterhus
## date: 30.07.2011
## version: A0.1
## input: measurement_vector, minimum value, maximum value
## output: list with: vector with flags (0 ok, -1 minimum error, +1 maximum error), minimum value, maximum value
	# initialisation of variables
	if (dim(measurement_vector)[1] > 0) {
		flagvector <- array(0.0, c(dim(measurement_vector)[1], dim(measurement_vector)[2]))
		# check every element of vector
		for (ii in 1:dim(measurement_vector)[1]) {
			for (jj in 1:dim(measurement_vector)[2]) {
				# just take values which are not NaN (prevent errors in R-routines))
				if (!is.na(measurement_vector[ii, jj])) {
					# check for minimum boundary
					if (!is.null(min_value)) {
						if (measurement_vector[ii, jj] < min_value) {
							flagvector[ii, jj] <- -1
						}
					}
					# check for maximum boundary
					if (!is.null(max_value)) {
						if (measurement_vector[ii, jj] > max_value) {
							flagvector[ii, jj] <- +1
						}
					}
				}
			}
		}
	}
	resultlist<- c(list(flagvector),list(min_value),list(max_value))
	names(resultlist)<-c("flagvector","min_value","max_value") 
	return(resultlist)
}
