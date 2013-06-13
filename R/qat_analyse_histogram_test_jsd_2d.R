qat_analyse_histogram_test_jsd_2d <-
function(measurement_vector, blocksize,numofbars, factorofbar) {
## functionality: calculates a field, which compares distributions of blocks of a measurement vector and compare them by Jensen-Shannon-Divergence
## author: André Düsterhus
## date: 22.03.2013
## version: A0.1
## input: measurement_vector
## output: list with a field, blocksize, numofbars
	resultlist <- qat_analyse_histogram_test_2d(measurement_vector, metric="JSD", blocksize=blocksize, numofbars=numofbars, factorofbar=factorofbar)
	return(resultlist)
}
