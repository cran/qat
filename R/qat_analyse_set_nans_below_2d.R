qat_analyse_set_nans_below_2d <-
function(measurement_vector, nan_below) {
## functionality: replace values below nan_below in measurement_vector with NaN
## author: André Düsterhus
## date: 21.04.2011
## version: A0.1
## input: measurement_vector, nan below value
## output: corrected measurement_vector
	measurement_vector[which(measurement_vector < nan_below)]<- NaN
	newlist <- list(measurement_vector=measurement_vector, nan_below=nan_below)
	return(newlist)
}
