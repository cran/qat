qat_analyse_trimmed_distribution_2d <-
function(measurement_vector) {
#library(moments)
## functionality: calculates trimmed moments of a measurement_vector
## author: André Düsterhus
## date: 02.08.2011
## version: A0.1
## input: measurement_vector
## output: list with a mean, standard deviation, skewness, kurtosis
	first_moment<-array(0.0, c(50, dim(measurement_vector)[2]))
	second_moment<-array(0.0, c(50, dim(measurement_vector)[2]))
	third_moment<-array(0.0, c(50, dim(measurement_vector)[2]))
	fourth_moment<-array(0.0, c(50, dim(measurement_vector)[2]))
	standard_deviation<-array(0.0, c(50, dim(measurement_vector)[2]))
	skewness<-array(0.0, c(50, dim(measurement_vector)[2]))
	kurtosis<-array(0.0, c(50, dim(measurement_vector)[2]))

	for (ii in 1:50) {
		for (jj in 1:dim(measurement_vector)[2]) {		
			measurement_vector_wo_nan <- measurement_vector[!is.na(measurement_vector[, jj]), jj]
			if(length(measurement_vector_wo_nan)>0) {	
				n <- length(measurement_vector_wo_nan)
				quantilevector<-seq(0,0.5,0.01)
				lowelem <- floor(n * quantilevector[ii]) + 1
				highelem <- n + 1 - lowelem
				vector_of_interest_wo_nan <- sort.int(measurement_vector_wo_nan, partial = unique(c(lowelem, highelem)))[lowelem:highelem]
				first_moment[ii, jj]<-moment(vector_of_interest_wo_nan, order=1, central=FALSE)
				second_moment[ii, jj]<-moment(vector_of_interest_wo_nan, order=2, central=TRUE)
				third_moment[ii, jj]<-moment(vector_of_interest_wo_nan, order=3, central=TRUE)
				fourth_moment[ii, jj]<-moment(vector_of_interest_wo_nan, order=4, central=TRUE)
				standard_deviation[ii, jj]<-sd(vector_of_interest_wo_nan)
				skewness[ii, jj]<-skewness(vector_of_interest_wo_nan)
				kurtosis[ii, jj]<-kurtosis(vector_of_interest_wo_nan)
			} else {
				first_moment[ii, jj] <- NaN
				second_moment[ii, jj]<-NaN			
				third_moment[ii, jj]<-NaN
				fourth_moment[ii, jj]<-NaN
				standard_deviation[ii, jj]<-NaN
				skewness[ii, jj]<-NaN
				kurtosis[ii, jj]<-NaN
			}
		}
	}
	resultliststat<- list()
	resultliststat<- c(list(first_moment),list(second_moment),list(third_moment),list(fourth_moment),list(standard_deviation),list(skewness), list(kurtosis))
	names(resultliststat)<-c("first_moment","second_moment","third_moment","fourth_moment", "standard_deviation","skewness", "kurtosis") 
	resultlist<- list(stat=resultliststat)
	return(resultlist)
}
