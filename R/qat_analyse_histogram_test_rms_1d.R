qat_analyse_histogram_test_rms_1d <-
function(measurement_vector, blocksize,numofbars) {
## functionality: calculates a field, which compares distributions of blocks of a measurement vector and compare them by root mean square
## author: André Düsterhus
## date: 29.09.2010
## version: A0.1
## input: measurement_vector
## output: list with a field, blocksize, numofbars
	resultlist <- qat_analyse_histogram_test_1d(measurement_vector, metric="RMS", blocksize=blocksize, numofbars=numofbars)
	return(resultlist)
}
