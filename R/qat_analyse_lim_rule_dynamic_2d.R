qat_analyse_lim_rule_dynamic_2d <-
function(measurement_vector, min_vector=NULL, max_vector=NULL, min_vector_name=NULL, max_vector_name=NULL, min_vector_identifier=NULL, max_vector_identifier=NULL) {
## functionality: check measurement_vector for dynamic lim rule
## author: André Düsterhus
## date: 30.07.2011
## version: A0.2
## input: measurement_vector, minimum vector, maximum vector, minimum vector name, maximum vector name, minimum vector identifier,
##        maximum vector identifier
## output: vector with flags (0 ok, -1 minimum error, +1 maximum error)
	# initialisation of variables
	flagvector <- array(0.0, c(dim(measurement_vector)[1], dim(measurement_vector)[2]))
	# check every element of vector
	if (!is.null(dim(min_vector))) {
		if((dim(measurement_vector)[1] != dim(min_vector)[1])||(dim(measurement_vector)[2] != dim(min_vector)[2]))  {
			min_vector <- array(NaN, c(dim(measurement_vector)[1], dim(measurement_vector)[2]))
		}
	} else {
		min_vector <- array(NaN, c(dim(measurement_vector)[1], dim(measurement_vector)[2]))
	}
	if (!is.null(max_vector)) {
		if(length(measurement_vector) != length(max_vector)) {
			max_vector <- array(NaN, c(dim(measurement_vector)[1], dim(measurement_vector)[2]))
		}
	} else {
		max_vector <- array(NaN, c(dim(measurement_vector)[1], dim(measurement_vector)[2]))		
	}
	for (ii in 1:dim(measurement_vector)[1]) {
		for (jj in 1:dim(measurement_vector)[2]) {
			# just take values which are not NaN (prevent errors in R-routines))
			if (!is.na(measurement_vector[ii, jj])) {
				if (!is.na(min_vector[ii, jj])) {
					# check for minimum boundary
					if (measurement_vector[ii, jj] < min_vector[ii, jj]) {
						flagvector[ii, jj] <- -1
					}
				}
				if (!is.na(max_vector[ii, jj])) {
					# check for maximum boundary
					if (measurement_vector[ii, jj] > max_vector[ii, jj]) {
						flagvector[ii, jj] <- +1
					}
				}
			}
		}
	}
	resultlist<- c(list(flagvector), list(min_vector), list(max_vector), list(min_vector_name), list(max_vector_name), list(min_vector_identifier), list(max_vector_identifier))
	names(resultlist)<-c("flagvector", "min_vector", "max_vector", "min_vector_name", "max_vector_name", "min_vector_identifier", "max_vector_identifier")
	return(resultlist)
}
