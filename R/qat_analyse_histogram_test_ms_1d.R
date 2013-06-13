qat_analyse_histogram_test_ms_1d <-
function(measurement_vector, blocksize,numofbars) {
## functionality: calculates a field, which compares distributions of blocks of a measurement vector and compare them by mean square
## author: André Düsterhus
## date: 08.12.2010
## version: A0.1
## input: measurement_vector
## output: list with a field, blocksize, numofbars
	resultlist <- qat_analyse_histogram_test_1d(measurement_vector, metric="MS", blocksize=blocksize, numofbars=numofbars)
	return(resultlist)
}
