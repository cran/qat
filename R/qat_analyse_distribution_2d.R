qat_analyse_distribution_2d <-
function(measurement_vector, numofbars) {
#library(moments)
## functionality: calculates the propability distributions of a measurement_vector
## author: André Düsterhus
## date: 02.08.2011
## version: A0.1
## input: measurement_vector
## output: list with a hist-element, 1st-4th moment, standard deviation, skewness, kurtosis

	resultlist <- list()
	measurement_vector_wo_nan <- as.vector(measurement_vector[!is.na(measurement_vector)])
	resultliststat<- list()
	if (length(measurement_vector_wo_nan)>0) {
		resultliststat[1] <- moment(measurement_vector_wo_nan, order=1, central=FALSE) 
		names(resultliststat)[1]<- "first_moment"
		resultliststat[2] <- moment(measurement_vector_wo_nan, order=2, central=TRUE) 
		names(resultliststat)[2]<- "second_moment"
		resultliststat[3] <- moment(measurement_vector_wo_nan, order=3, central=TRUE) 
		names(resultliststat)[3]<- "third_moment"
		resultliststat[4] <- moment(measurement_vector_wo_nan, order=4, central=TRUE) 
		names(resultliststat)[4]<- "fourth_moment"
		resultliststat[5] <- sd(measurement_vector_wo_nan) 
		names(resultliststat)[5]<- "standard_deviation"
		resultliststat[6] <- skewness(measurement_vector_wo_nan) 
		names(resultliststat)[6]<- "skewness"
		resultliststat[7] <- kurtosis(measurement_vector_wo_nan) 
		names(resultliststat)[7]<- "kurtosis"
		resultliststat[8] <- quantile(measurement_vector_wo_nan, probs=0.5) 
		names(resultliststat)[8]<- "median"
		resultliststat[9] <- quantile(measurement_vector_wo_nan, probs=0.05) 
		names(resultliststat)[9]<- "p5_quantile"
		resultliststat[10] <- quantile(measurement_vector_wo_nan, probs=0.95) 
		names(resultliststat)[10]<- "p95_quantile"
		resultliststat[11] <- quantile(measurement_vector_wo_nan, probs=0.25) 
		names(resultliststat)[11]<- "p25_quantile"
		resultliststat[12] <- quantile(measurement_vector_wo_nan, probs=0.75) 
		names(resultliststat)[12]<- "p75_quantile"
		resultlisthist <- hist(measurement_vector_wo_nan, breaks=seq(min(measurement_vector_wo_nan), max(measurement_vector_wo_nan), length=numofbars+1), include.lowest=TRUE, plot=FALSE)
	} else {
		resultlisthist <- NaN
		resultliststat <- NaN
	}
	resultlist <- list(hist=resultlisthist,stat=resultliststat, numofbars=numofbars)
	return(resultlist)
}
