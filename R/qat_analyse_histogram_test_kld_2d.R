qat_analyse_histogram_test_kld_2d <-
function(measurement_vector, blocksize=floor(length(measurement_vector)/20), numofbars=65, factorofbar=100) {
## functionality: calculates a field, which compares distributions of blocks of a measurement vector and compare them by Kullback-Leibler-Divergence
## author: André Düsterhus
## date: 22.03.2013
## version: A0.1
## input: measurement_vector
## output: list with a field, blocksize, numofbars
	# calculating epsilon, which will be used later on as a replacement for data2=0
	if (is.null(blocksize)) {
		blocksize <- floor(length(measurement_vector)/20)
	}
	if (is.null(numofbars)) {
		numofbars <- 65
	}
	if (is.null(blocksize)) {
		factorofbar <- 100
	}
	epsilon <- 1 / (blocksize*factorofbar)
	# calculating the number of blocks
	runs<-floor(dim(measurement_vector)[1]/blocksize)
	# initialisation of the array, which should store the result
	resultarray<- array(0.0,c(runs,runs))
	# cycles over all block, so that each block get compared with each other
	minimum <- min(measurement_vector, na.rm=TRUE)
	maximum <- max(measurement_vector, na.rm=TRUE)
	for (ii in 1:runs) {
		for (jj in 1:runs) {
			# producing two vectors of data of length blocksize, which should be analysed in this step
			data1 <- measurement_vector[((ii-1)*blocksize+1):(ii*blocksize),]
			data2 <- measurement_vector[((jj-1)*blocksize+1):(jj*blocksize),]
			# calculating the individual maximum and minimum of the two vectors and constructing a vector of breaks
			breakvector<- seq(minimum,maximum,length.out=numofbars)
			# calculating histograms of both data-vectors with the given breakvector
			hdata1<-hist(data1, breaks= breakvector,plot=FALSE)
			hdata2<-hist(data2, breaks= breakvector,plot=FALSE)
			# extracting densities
			h1density<-hdata1$counts/sum(hdata1$counts)
			h2density<-hdata2$counts/sum(hdata2$counts)
			zz<-which((h1density==0))
			# elements of h2density, which are greater than 0 need no epsilon
			xx<-which((h2density>0))
			yy<-setdiff(1:length(h1density),xx)
			if(length(yy)>0) {
				# adding epsilon and renormaize the density-vectors
				h1density <- h1density + epsilon
				h1density <- h1density/sum(h1density)
				h2density <- h2density + epsilon
				h2density <- h2density/sum(h2density)
			}
			# initialise result
			result <- array(0.0,length(numofbars))
			# fill in kullback leibler divergence for elements, where there is a contribution from any 
			xx<-which((h1density>0) & (h2density>0))
			result[xx] <- h1density[xx]*log2(h1density[xx]/h2density[xx])
			# reassuring that every elmenet, where the first datavector indicates no entry is set to 0
			result[which((h1density==0))] <- 0.0
			# the resultvector with results for every bar have to be summed up to get a result for the whole distribution
			resultarray[ii,jj] <- sum(result)
		}
	}
	resultlist <- list(field=resultarray, blocksize=blocksize, numofbars=numofbars, factorofbar=factorofbar, metric="kld", runs=runs)
	return(resultlist)
}
