qat_analyse_set_mean_1d <-
function(measurement_vector, blocksize) {
## functionality: calculates mean of blocks of measurement_vector
## author: André Düsterhus
## date: 12.02.2010
## version: A0.1
## input: measurement_vector, block_size
## output: vector of mean values
	mean_vector<- array(0.0,floor(length(measurement_vector)/blocksize))
	for (ii in 1:floor(length(measurement_vector)/blocksize)) {
		mean_vector[ii]<- mean(measurement_vector[((ii-1)*blocksize+1):(ii*blocksize)],na.rm =TRUE)
	}
	newlist <- list(measurement_vector=mean_vector, blocksize=blocksize)
	return(newlist)
}

