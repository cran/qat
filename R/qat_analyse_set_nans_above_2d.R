qat_analyse_set_nans_above_2d <-
function(measurement_vector, nan_above) {
## functionality: replace values above nan_above in measurement_vector with NaN
## author: André Düsterhus
## date: 21.04.2011
## version: A0.1
## input: measurement_vector, nan above value
## output: corrected measurement_vector
	measurement_vector[which(measurement_vector > nan_above)]<- NaN
	newlist <- list(measurement_vector=measurement_vector, nan_above=nan_above)
	return(newlist)
}
