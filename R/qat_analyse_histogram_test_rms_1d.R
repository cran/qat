qat_analyse_histogram_test_rms_1d <-
function(measurement_vector, blocksize,numofbars) {
## functionality: calculates a field, which compares distributions of blocks of a measurement vector and compare them by root mean square
## author: André Düsterhus
## date: 29.09.2010
## version: A0.1
## input: measurement_vector
## output: list with a field, blocksize, numofbars
	# calculating the number of blocks
	runs<-floor(length(measurement_vector)/blocksize)
	# initialisation of the array, which should store the result
	resultarray<- array(0.0,c(runs,runs))
	minimum <- min(measurement_vector, na.rm=TRUE)
	maximum <- max(measurement_vector, na.rm=TRUE)
	# cycles over all block, so that each block get compared with each other
	for (ii in 1:runs) {
		for (jj in 1:runs) {
			# producing two vectors of data of length blocksize, which should be analysed in this step
			data1 <- measurement_vector[((ii-1)*blocksize+1):(ii*blocksize)]
			data2 <- measurement_vector[((jj-1)*blocksize+1):(jj*blocksize)]
			# calculating the individual maximum and minimum of the two vectors and constructing a vector of breaks
			breakvector<- seq(minimum,maximum,length.out=numofbars)
			# calculating histograms of both data-vectors with the given breakvector
			hdata1<-hist(data1, breaks= breakvector,plot=FALSE)
			hdata2<-hist(data2, breaks= breakvector,plot=FALSE)
			# extracting densities
			h1density<-hdata1$counts/sum(hdata1$counts)
			h2density<-hdata2$counts/sum(hdata2$counts)
			result <- array(0.0,length(numofbars))
			result <- (h1density-h2density)^2
			resultarray[ii,jj] <- sqrt(sum(result))
		}
	}
	resultlist <- list(field=resultarray, blocksize=blocksize,numofbars=numofbars, metric="rms", runs=runs)
	return(resultlist)
}
