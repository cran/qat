qat_analyse_noc_rule_2d <-
function(measurement_vector, max_return_elements = NULL) {
## functionality: check measurement_vector for dynamic lim rule
## author: André Düsterhus
## date: 30.07.2011
## version: A0.1
## input: measurement_vector, max_return_elements
## output: vector with flags (0 ok, +1 repetition error)
	# initialisation of variables
	flagvector <- array(0.0, c(dim(measurement_vector)[1], dim(measurement_vector)[2]))
	if (!is.null(max_return_elements)) {
		# check every element of vector
		for (ii in (max_return_elements+1):dim(measurement_vector)[1]) {
			for (kk in 1:dim(measurement_vector)[2]) {
				# just take values which are not NaN (prevent errors in R-routines))
				if (!is.na(measurement_vector[ii, kk])) {
					bool = TRUE 
					for (jj in (ii-1):(ii-max_return_elements)) {
						if (!is.na(measurement_vector[jj, kk])) {
							if (measurement_vector[ii, kk] != measurement_vector[jj, kk]) {
								bool = FALSE
							}
						} else {
							bool = FALSE
						}
					}
					if (bool == TRUE) {
						flagvector[ii, kk] = 1
					}
				}
			}
		}
	}
	resultlist<- c(list(flagvector),list(max_return_elements))
	names(resultlist)<-c("flagvector","max_return_elements")
	return(resultlist)
}
