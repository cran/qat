qat_analyse_boot_distribution_1d <-
function(measurement_vector, bootruns) {
#library("boot")     
#library("moments")
## functionality: check statistical moments of the measurement_vector with bootstrapping technique
## author: André Düsterhus
## date: 03.06.2010
## version: A0.1
## input: measurement_vector, bootruns
## output: list with: moments of each run, bootruns
	# using a function with minimal effect to get the indices of the boot-function
	foo <- function(d, ii) mean(d[ii])
	elements<-boot(measurement_vector, foo, R=bootruns) 
	boot_elements<-boot.array(elements,indices=TRUE)
	# calculating statistical moments for each run	
	first_moment <- apply (array(measurement_vector[boot_elements],c(length(boot_elements[,1]), length(boot_elements[1,]))), MARGIN=1, function(x) moment(x, order=1, central=FALSE, na.rm = TRUE))
	second_moment <- apply (array(measurement_vector[boot_elements],c(length(boot_elements[,1]), length(boot_elements[1,]))), MARGIN=1, function(x) moment(x, order=2, central=TRUE, na.rm = TRUE))
	third_moment <- apply (array(measurement_vector[boot_elements],c(length(boot_elements[,1]), length(boot_elements[1,]))), MARGIN=1, function(x) moment(x, order=3, central=TRUE, na.rm = TRUE))
	fourth_moment <- apply (array(measurement_vector[boot_elements],c(length(boot_elements[,1]), length(boot_elements[1,]))), MARGIN=1, function(x) moment(x, order=4, central=TRUE, na.rm = TRUE))
	standard_deviation<-apply (array(measurement_vector[boot_elements],c(length(boot_elements[,1]), length(boot_elements[1,]))), MARGIN=1, function(x) sd(x, na.rm=TRUE))
	skewness<-apply (array(measurement_vector[boot_elements],c(length(boot_elements[,1]), length(boot_elements[1,]))), MARGIN=1, function(x) skewness(x, na.rm=TRUE))
	kurtosis<-apply (array(measurement_vector[boot_elements],c(length(boot_elements[,1]), length(boot_elements[1,]))), MARGIN=1, function(x) kurtosis(x, na.rm=TRUE))
	median <- apply (array(measurement_vector[boot_elements],c(length(boot_elements[,1]), length(boot_elements[1,]))), MARGIN=1, function(x) quantile(x, probs=0.5, na.rm = TRUE))
	p5quantile <- apply (array(measurement_vector[boot_elements],c(length(boot_elements[,1]), length(boot_elements[1,]))), MARGIN=1, function(x) quantile(x, probs=0.05, na.rm = TRUE))
	p25quantile <- apply (array(measurement_vector[boot_elements],c(length(boot_elements[,1]), length(boot_elements[1,]))), MARGIN=1, function(x) quantile(x, probs=0.25, na.rm = TRUE))
	p75quantile <- apply (array(measurement_vector[boot_elements],c(length(boot_elements[,1]), length(boot_elements[1,]))), MARGIN=1, function(x) quantile(x, probs=0.75, na.rm = TRUE))
	p95quantile <- apply (array(measurement_vector[boot_elements],c(length(boot_elements[,1]), length(boot_elements[1,]))), MARGIN=1, function(x) quantile(x, probs=0.95, na.rm = TRUE))
	# set up resultlist
	resultliststat<- list()
	resultliststat<- c(list(first_moment),list(second_moment),list(third_moment),list(fourth_moment),list(standard_deviation),list(skewness), list(kurtosis), list(median), list(p5quantile), list(p95quantile), list(p25quantile), list(p75quantile))
	names(resultliststat)<-c("first_moment","second_moment","third_moment","fourth_moment", "standard_deviation","skewness", "kurtosis","median", "p5_quantile", "p95_quantile", "p25_quantile","p75_quantile") 
	resultlist<- list(stat=resultliststat,bootruns=bootruns)
	return(resultlist)
}
