qat_analyse_set_nans_1d <-
function(measurement_vector, nan_value) {
## functionality: replace nan_value in measurement_vector with NaN
## author: André Düsterhus
## date: 28.11.2009
## version: A0.1
## input: measurement_vector, nan value
## output: corrected measurement_vector
	measurement_vector[which(measurement_vector == nan_value)]<- NaN
	newlist <- list(measurement_vector=measurement_vector, nan_value=nan_value)
	return(newlist)
}

