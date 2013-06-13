qat_analyse_histogram_test_kld_2d <-
function(measurement_vector, blocksize=floor(length(measurement_vector)/20), numofbars=65, factorofbar=100) {
## functionality: calculates a field, which compares distributions of blocks of a measurement vector and compare them by Kullback-Leibler-Divergence
## author: André Düsterhus
## date: 22.03.2013
## version: A0.1
## input: measurement_vector
## output: list with a field, blocksize, numofbars
	resultlist <- qat_analyse_histogram_test_2d(measurement_vector, metric="KLD", blocksize=blocksize, numofbars=numofbars, factorofbar=factorofbar)
	return(resultlist)
}
