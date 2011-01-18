qat_analyse_set_addup_1d <-
function(measurement_vector, blocksize) {
## functionality: calculates cumulative sums of measurement_vector
## author: André Düsterhus
## date: 12.02.2010
## version: A0.1
## input: measurement_vector, block_size
## output: vector of cumulative sums
	cumsum_vector<- array(0.0,floor(length(measurement_vector)/blocksize))
	for (ii in 1:floor(length(measurement_vector)/blocksize)) {
		cumsum_vector[ii]<- sum(measurement_vector[((ii-1)*blocksize+1):(ii*blocksize)],na.rm =TRUE)
	}
	newlist <- list(measurement_vector=cumsum_vector, blocksize=blocksize)
	return(newlist)
}
