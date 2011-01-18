qat_analyse_block_distribution_1d <-
function(measurement_vector, blocksize) {
library(moments)
## functionality: calculates the propability distributions of blockwise sections of a measurement_vector
## author: André Düsterhus
## date: 30.01.2010
## version: A0.1
## input: measurement_vector, number of elements of the block
## output: list with a hist-element, 1st-4th moment, standard deviation, skewness, kurtosis
	numof_blocks<-floor(length(measurement_vector)/ blocksize)
	vector_of_interest <- array(0.0,blocksize)
	first_moment<-array(0.0,numof_blocks)
	second_moment<-array(0.0,numof_blocks)
	third_moment<-array(0.0,numof_blocks)
	fourth_moment<-array(0.0,numof_blocks)
	standard_deviation<-array(0.0,numof_blocks)
	skewness<-array(0.0,numof_blocks)
	kurtosis<-array(0.0,numof_blocks)
	median<-array(0.0,numof_blocks)
	p5quantile<-array(0.0,numof_blocks)
	p95quantile<-array(0.0,numof_blocks)
	p25quantile<-array(0.0,numof_blocks)
	p75quantile<-array(0.0,numof_blocks)

	for (ii in 1:numof_blocks) {
		vector_of_interest <- measurement_vector[((ii-1)*blocksize+1):(ii*blocksize)]
		vector_of_interest_wo_nan <- vector_of_interest[!is.na(vector_of_interest)]
#		print(is.na(vector_of_interest))
#		print(vector_of_interest)
#		print(union(which(!is.nan(vector_of_interest)),which(!is.na(vector_of_interest))))
		if(length(vector_of_interest_wo_nan)>0) {
			first_moment[ii]<-moment(vector_of_interest_wo_nan, order=1, central=FALSE)
			second_moment[ii]<-moment(vector_of_interest_wo_nan, order=2, central=TRUE)
			third_moment[ii]<-moment(vector_of_interest_wo_nan, order=3, central=TRUE)
			fourth_moment[ii]<-moment(vector_of_interest_wo_nan, order=4, central=TRUE)
			standard_deviation[ii]<-sd(vector_of_interest_wo_nan)
			skewness[ii]<-skewness(vector_of_interest_wo_nan)
			kurtosis[ii]<-kurtosis(vector_of_interest_wo_nan)
			median[ii]<-quantile(vector_of_interest_wo_nan, probs=0.5)
			p5quantile[ii]<-quantile(vector_of_interest_wo_nan, probs=0.05)
			p95quantile[ii]<-quantile(vector_of_interest_wo_nan, probs=0.95)
			p25quantile[ii]<-quantile(vector_of_interest_wo_nan, probs=0.25)
			p75quantile[ii]<-quantile(vector_of_interest_wo_nan, probs=0.75)
		} else {
			first_moment[ii] <- NaN
			second_moment[ii]<-NaN			
			third_moment[ii]<-NaN
			fourth_moment[ii]<-NaN
			standard_deviation[ii]<-NaN
			skewness[ii]<-NaN
			kurtosis[ii]<-NaN
			median[ii]<-NaN
			p5quantile[ii]<-NaN
			p95quantile[ii]<-NaN
			p25quantile[ii]<-NaN
			p75quantile[ii]<-NaN
		}
	}
	resultliststat<- list()
	resultliststat<- c(list(first_moment),list(second_moment),list(third_moment),list(fourth_moment),list(standard_deviation),list(skewness), list(kurtosis), list(median), list(p5quantile), list(p95quantile), list(p25quantile), list(p75quantile))
	names(resultliststat)<-c("first_moment","second_moment","third_moment","fourth_moment", "standard_deviation","skewness", "kurtosis","median", "p5_quantile", "p95_quantile", "p25_quantile","p75_quantile") 
	resultlist<- list(stat=resultliststat,blocksize=blocksize)
	return(resultlist)
}
