qat_analyse_block_distribution_2d <-
function(measurement_vector, blocksize) {
#library(moments)
## functionality: calculates the propability distributions of blockwise sections of a measurement_vector
## author: André Düsterhus
## date: 02.08.2011
## version: A0.1
## input: measurement_vector, number of elements of the block
## output: list with a hist-element, 1st-4th moment, standard deviation, skewness, kurtosis
	numof_blocks<-floor(dim(measurement_vector)[1]/ blocksize)
	vector_of_interest <- array(0.0,c(blocksize, dim(measurement_vector)[2]))
	first_moment<-array(0.0, c(numof_blocks, dim(measurement_vector)[2]))
	second_moment<-array(0.0, c(numof_blocks, dim(measurement_vector)[2]))
	third_moment<-array(0.0, c(numof_blocks, dim(measurement_vector)[2]))
	fourth_moment<-array(0.0, c(numof_blocks, dim(measurement_vector)[2]))
	standard_deviation<-array(0.0, c(numof_blocks, dim(measurement_vector)[2]))
	skewness<-array(0.0, c(numof_blocks, dim(measurement_vector)[2]))
	kurtosis<-array(0.0, c(numof_blocks, dim(measurement_vector)[2]))
	median<-array(0.0, c(numof_blocks, dim(measurement_vector)[2]))
	p5quantile<-array(0.0, c(numof_blocks, dim(measurement_vector)[2]))
	p95quantile<-array(0.0, c(numof_blocks, dim(measurement_vector)[2]))
	p25quantile<-array(0.0, c(numof_blocks, dim(measurement_vector)[2]))
	p75quantile<-array(0.0, c(numof_blocks, dim(measurement_vector)[2]))

	for (ii in 1:numof_blocks) {
		for (jj in 1:dim(measurement_vector)[2]) {
			vector_of_interest <- measurement_vector[((ii-1)*blocksize+1):(ii*blocksize), jj]
			vector_of_interest_wo_nan <- vector_of_interest[!is.na(vector_of_interest)]
	#		print(is.na(vector_of_interest))
	#		print(vector_of_interest)
	#		print(union(which(!is.nan(vector_of_interest)),which(!is.na(vector_of_interest))))
			if(length(vector_of_interest_wo_nan)>0) {
				first_moment[ii, jj]<-moment(vector_of_interest_wo_nan, order=1, central=FALSE)
				second_moment[ii, jj]<-moment(vector_of_interest_wo_nan, order=2, central=TRUE)
				third_moment[ii, jj]<-moment(vector_of_interest_wo_nan, order=3, central=TRUE)
				fourth_moment[ii, jj]<-moment(vector_of_interest_wo_nan, order=4, central=TRUE)
				standard_deviation[ii, jj]<-sd(vector_of_interest_wo_nan)
				skewness[ii, jj]<-skewness(vector_of_interest_wo_nan)
				kurtosis[ii, jj]<-kurtosis(vector_of_interest_wo_nan)
				median[ii, jj]<-quantile(vector_of_interest_wo_nan, probs=0.5)
				p5quantile[ii, jj]<-quantile(vector_of_interest_wo_nan, probs=0.05)
				p95quantile[ii, jj]<-quantile(vector_of_interest_wo_nan, probs=0.95)
				p25quantile[ii, jj]<-quantile(vector_of_interest_wo_nan, probs=0.25)
				p75quantile[ii, jj]<-quantile(vector_of_interest_wo_nan, probs=0.75)
			} else {
				first_moment[ii, jj] <- NaN
				second_moment[ii, jj]<-NaN			
				third_moment[ii, jj]<-NaN
				fourth_moment[ii, jj]<-NaN
				standard_deviation[ii, jj]<-NaN
				skewness[ii, jj]<-NaN
				kurtosis[ii, jj]<-NaN
				median[ii, jj]<-NaN
				p5quantile[ii, jj]<-NaN
				p95quantile[ii, jj]<-NaN
				p25quantile[ii, jj]<-NaN
				p75quantile[ii, jj]<-NaN
			}
		}
	}
	resultliststat<- list()
	resultliststat<- c(list(first_moment),list(second_moment),list(third_moment),list(fourth_moment),list(standard_deviation),list(skewness), list(kurtosis), list(median), list(p5quantile), list(p95quantile), list(p25quantile), list(p75quantile))
	names(resultliststat)<-c("first_moment","second_moment","third_moment","fourth_moment", "standard_deviation","skewness", "kurtosis","median", "p5_quantile", "p95_quantile", "p25_quantile","p75_quantile") 
	resultlist<- list(stat=resultliststat,blocksize=blocksize)
	return(resultlist)
}
