qat_call_trimmed_distribution <-
function(measurement_vector, workflowlist_part, element=-999, time=NULL, height= NULL, lat=NULL, lon=NULL, vec1=NULL,vec2=NULL,vec3=NULL,vec4=NULL,resultlist=list(), resultlistcounter=1) {
## functionality: calling function for qat_analyse_trimmed_distribution
## author: André Düsterhus
## date: 04.05.2010
## version: A0.1
## input: measurement_vector, workflowlist element, number of actual element, time vector (optional), latitude vector (optional), longitude vector (optional), 4 optional vectors, resultlist (optional), counter of resultlist (optional)
## output: list with the results and parameters of the distribution
	if (is.null(dim(measurement_vector))) {
		resultlist[[resultlistcounter <- resultlistcounter+1]] <- list(element=element, method='dist_trimmed', result =qat_analyse_trimmed_distribution_1d(measurement_vector))
	}
	if (length(dim(measurement_vector))==2) {
		resultlist[[resultlistcounter <- resultlistcounter+1]] <- list(element=element, method='dist_trimmed', result =qat_analyse_trimmed_distribution_2d(measurement_vector))
	}	
	return(resultlist)
}
