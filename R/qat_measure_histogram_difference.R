qat_measure_histogram_difference <-
function(data1, data2, metric="EMD", breakvector=NULL, numofbars=65, factorofbar=100) {
## functionality: calculates the difference of the histograms of two given datasets
## author: André Düsterhus
## date: 04.05.2013
## version: A0.1
## input: measurement_vector, co measurement vector, metric identifier, breakvector, number of bars, factor of bars
## output: list with a field, blocksize, numofbars
	if (is.null(breakvector)) {
		breakvector <- seq(min(c(data1, data2)), max(c(data1, data2)), length.out=numofbars+1)
	}	
	# calculating histograms of both data-vectors with the given breakvector
	hdata1<-hist(as.vector(data1), breaks= breakvector,plot=FALSE)
	hdata2<-hist(as.vector(data2), breaks= breakvector,plot=FALSE)
	if (is.element(metric, c("EMD", "CRPS"))) {
		# extracting densities
		h1density<-cumsum(hdata1$counts/sum(hdata1$counts))
		h2density<-cumsum(hdata2$counts/sum(hdata2$counts))
	}
	if (is.element(metric, c("KLD", "JSD", "RMS", "MS"))) {
		# extracting densities
		h1density<-hdata1$counts/sum(hdata1$counts)
		h2density<-hdata2$counts/sum(hdata2$counts)
	}	
	if (is.element(metric, c("KLD"))) {
		epsilon <- 1 / (sum(hdata1$counts)*factorofbar)
		if (length(which(h2density==0)) >0) {
			h1density <- (hdata1$counts+epsilon)/(sum(hdata1$counts)+(length(breakvector)-1)*epsilon)
			h2density <- (hdata2$counts+epsilon)/(sum(hdata2$counts)+(length(breakvector)-1)*epsilon)
		}
	}		
	if (is.element(metric, c("JSD"))) {
		epsilon <- 1 / (sum(hdata1$counts)*factorofbar)
		if ((length(which(h2density==0)) >0)||((length(which(h1density==0))))) {
			h1density <- (hdata1$counts+epsilon)/(sum(hdata1$counts)+(length(breakvector)-1)*epsilon)
			h2density <- (hdata2$counts+epsilon)/(sum(hdata2$counts)+(length(breakvector)-1)*epsilon)
		}
	}

	if (is.element(metric, c("KLD"))) {
		result_pre <- array(0.0,length(breakvector)-1)
		# fill in kullback leibler divergence for elements, where there is a contribution from any 
		xx<-which((h1density>0) & (h2density>0))
		result_pre[xx] <- h1density[xx]*log2(h1density[xx]/h2density[xx])
		# reassuring that every elmenet, where the first datavector indicates no entry is set to 0
		result_pre[which((h1density==0))] <- 0.0
		result <- sum(result_pre)
	}
	if (is.element(metric, c("JSD"))) {
		hmixdensity <- 1/2*(h1density + h2density)
		result <- sum(1/2*h1density*log2(h1density/hmixdensity)+1/2*h2density*log2(h2density/hmixdensity))
	}
	if (is.element(metric, c("RMS"))) {
		result <- sqrt(1/(length(breakvector)-1) * sum((h1density-h2density)^2))
	}		
	if (is.element(metric, c("MS"))) {
		result <- 1/(length(breakvector)-1) * sum((h1density-h2density)^2)
	}	
	if (is.element(metric, c("EMD"))) {
		result <- 1/(length(breakvector)-1) * sum(abs(h1density-h2density))
	}
	if (is.element(metric, c("CRPS"))) {
		result <- 1/(length(breakvector)-1) * sum(abs(h1density-h2density)^2)
	}	
	return(result)
}
