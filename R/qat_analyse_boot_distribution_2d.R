qat_analyse_boot_distribution_2d <-
function(measurement_vector, bootruns) {
library("boot")     
library("moments")
## functionality: check statistical moments of the measurement_vector with bootstrapping technique
## author: André Düsterhus
## date: 02.08.2011
## version: A0.1
## input: measurement_vector, bootruns
## output: list with: moments of each run, bootruns
	# using a function with minimal effect to get the indices of the boot-function
	first_moment <- array(0.0, c(bootruns, dim(measurement_vector)[2]))
	second_moment <- array(0.0, c(bootruns, dim(measurement_vector)[2]))
	third_moment <- array(0.0, c(bootruns, dim(measurement_vector)[2]))
	fourth_moment <- array(0.0, c(bootruns, dim(measurement_vector)[2]))
	standard_deviation <- array(0.0, c(bootruns, dim(measurement_vector)[2]))
	skewness <- array(0.0, c(bootruns, dim(measurement_vector)[2]))
	kurtosis <- array(0.0, c(bootruns, dim(measurement_vector)[2]))
	median <- array(0.0, c(bootruns, dim(measurement_vector)[2]))
	p5quantile <- array(0.0, c(bootruns, dim(measurement_vector)[2]))
	p95quantile <- array(0.0, c(bootruns, dim(measurement_vector)[2]))
	p25quantile <- array(0.0, c(bootruns, dim(measurement_vector)[2]))
	p75quantile <- array(0.0, c(bootruns, dim(measurement_vector)[2]))	
	
	foo <- function(d, ii) mean(d[ii])
	for (jj in 1:dim(measurement_vector)[2]) {
		elements<-boot(measurement_vector[,jj], foo, R=bootruns) 
		boot_elements<-boot.array(elements,indices=TRUE)
	# calculating statistical moments for each run
		first_moment[, jj] <- apply (array(measurement_vector[boot_elements, jj],c(length(boot_elements[,1]), length(boot_elements[1,]))), MARGIN=1, function(x) moment(x, order=1, central=FALSE, na.rm = TRUE))
		second_moment[, jj] <- apply (array(measurement_vector[boot_elements, jj],c(length(boot_elements[,1]), length(boot_elements[1,]))), MARGIN=1, function(x) moment(x, order=2, central=TRUE, na.rm = TRUE))
		third_moment[, jj] <- apply (array(measurement_vector[boot_elements, jj],c(length(boot_elements[,1]), length(boot_elements[1,]))), MARGIN=1, function(x) moment(x, order=3, central=TRUE, na.rm = TRUE))
		fourth_moment[, jj] <- apply (array(measurement_vector[boot_elements, jj],c(length(boot_elements[,1]), length(boot_elements[1,]))), MARGIN=1, function(x) moment(x, order=4, central=TRUE, na.rm = TRUE))
		standard_deviation[, jj] <-apply (array(measurement_vector[boot_elements, jj],c(length(boot_elements[,1]), length(boot_elements[1,]))), MARGIN=1, function(x) sd(x, na.rm=TRUE))
		skewness[, jj] <-apply (array(measurement_vector[boot_elements, jj],c(length(boot_elements[,1]), length(boot_elements[1,]))), MARGIN=1, function(x) skewness(x, na.rm=TRUE))
		kurtosis[, jj] <-apply (array(measurement_vector[boot_elements, jj],c(length(boot_elements[,1]), length(boot_elements[1,]))), MARGIN=1, function(x) kurtosis(x, na.rm=TRUE))
		median[, jj] <- apply (array(measurement_vector[boot_elements, jj],c(length(boot_elements[,1]), length(boot_elements[1,]))), MARGIN=1, function(x) quantile(x, probs=0.5, na.rm = TRUE))
		p5quantile[, jj] <- apply (array(measurement_vector[boot_elements, jj],c(length(boot_elements[,1]), length(boot_elements[1,]))), MARGIN=1, function(x) quantile(x, probs=0.05, na.rm = TRUE))
		p25quantile[, jj] <- apply (array(measurement_vector[boot_elements, jj],c(length(boot_elements[,1]), length(boot_elements[1,]))), MARGIN=1, function(x) quantile(x, probs=0.25, na.rm = TRUE))
		p75quantile[, jj] <- apply (array(measurement_vector[boot_elements, jj],c(length(boot_elements[,1]), length(boot_elements[1,]))), MARGIN=1, function(x) quantile(x, probs=0.75, na.rm = TRUE))
		p95quantile[, jj] <- apply (array(measurement_vector[boot_elements, jj],c(length(boot_elements[,1]), length(boot_elements[1,]))), MARGIN=1, function(x) quantile(x, probs=0.95, na.rm = TRUE))
	}
	# set up resultlist
	resultliststat<- list()
	resultliststat<- c(list(first_moment),list(second_moment),list(third_moment),list(fourth_moment),list(standard_deviation),list(skewness), list(kurtosis), list(median), list(p5quantile), list(p95quantile), list(p25quantile), list(p75quantile))
	names(resultliststat)<-c("first_moment","second_moment","third_moment","fourth_moment", "standard_deviation","skewness", "kurtosis","median", "p5_quantile", "p95_quantile", "p25_quantile","p75_quantile") 
	resultlist<- list(stat=resultliststat,bootruns=bootruns)
	return(resultlist)
}
