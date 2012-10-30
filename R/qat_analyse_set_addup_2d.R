qat_analyse_set_addup_2d <-
function(measurement_vector, blocksize) {
## functionality: calculates cumulative sums of measurement_vector
## author: André Düsterhus
## date: 12.02.2010
## version: A0.1
## input: measurement_vector, block_size
## output: vector of cumulative sums
	cumsum_vector <- array(0.0,c(dim(measurement_vector)[1], floor(dim(measurement_vector)[2]/blocksize)))
	for (jj in 1:dim(measurement_vector)[1]) {
		for (ii in 1:floor(dim(measurement_vector)[2]/blocksize)) {
			cumsum_vector[jj, ii]<- sum(measurement_vector[jj,((ii-1)*blocksize+1):(ii*blocksize)],na.rm =TRUE)
		}
	}
	newlist <- list(measurement_vector=cumsum_vector, blocksize=blocksize)
	return(newlist)
}
