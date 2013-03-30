qat_analyse_histogram_test_jsd_1d <-
function(measurement_vector, blocksize,numofbars, factorofbar) {
## functionality: calculates a field, which compares distributions of blocks of a measurement vector and compare them by Jensen-Shannon-Divergence
## author: André Düsterhus
## date: 29.09.2010
## version: A0.1
## input: measurement_vector
## output: list with a field, blocksize, numofbars
	# calculating epsilon, which will be used later on as a replacement for data2=0
	epsilon <- 1 / (blocksize*factorofbar)
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
			hmixdensity <- 1/2*(h1density + h2density)
			result[xx] <- 1/2*h1density[xx]*log2(h1density[xx]/hmixdensity[xx])+1/2*h2density[xx]*log2(h2density[xx]/hmixdensity[xx])
			# reassuring that every elmenet, where the first datavector indicates no entry is set to 0
			result[which((h1density==0))] <- 0.0
			result[which((h2density==0))] <- 0.0
			# the resultvector with results for every bar have to be summed up to get a result for the whole distribution
			resultarray[ii,jj] <- sum(result)
		}
	}
	resultlist <- list(field=resultarray, blocksize=blocksize,numofbars=numofbars, factorofbar=factorofbar, metric="jsd", runs=runs)
	return(resultlist)
}
