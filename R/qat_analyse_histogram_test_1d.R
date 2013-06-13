qat_analyse_histogram_test_1d <-
function(measurement_vector, co_measurement_vector=measurement_vector, metric="EMD", blocksize=100, numofbars=65, factorofbar=100) {
## functionality: calculates a field, which compares distributions of blocks of a measurement vector and compare them by a given metric
## author: André Düsterhus
## date: 04.05.2013
## version: A0.1
## input: measurement_vector, co measurement vector, metric identifier, blocksize, number of bars, factor of bars
## output: list with a field, blocksize, numofbars
	# calculating the number of blocks
	runs<-floor(length(measurement_vector)/blocksize)
	# initialisation of the array, which should store the result
	resultarray<- array(0.0,c(runs,runs))
	minimum <- min(c(measurement_vector, co_measurement_vector), na.rm=TRUE)
	maximum <- max(c(measurement_vector, co_measurement_vector), na.rm=TRUE)
	# calculating the individual maximum and minimum of the two vectors and constructing a vector of breaks
	breakvector<- seq(minimum,maximum,length.out=numofbars+1)
	# cycles over all block, so that each block get compared with each other
	for (ii in 1:runs) {
		for (jj in 1:runs) {
			# producing two vectors of data of length blocksize, which should be analysed in this step
			data1 <- measurement_vector[((ii-1)*blocksize+1):(ii*blocksize)]
			data2 <- co_measurement_vector[((jj-1)*blocksize+1):(jj*blocksize)]
			resultarray[ii,jj] <- qat_measure_histogram_difference(data1, data2, metric=metric, breakvector=breakvector, factorofbar=factorofbar)
		}
	}
	resultlist <- list(field=resultarray, blocksize=blocksize, numofbars=numofbars, factorofbar=factorofbar, metric=metric, runs=runs)
	return(resultlist)
}
