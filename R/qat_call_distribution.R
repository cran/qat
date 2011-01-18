qat_call_distribution <-
function(measurement_vector, workflowlist_part, element=-999, time=NULL, height= NULL, lat=NULL, lon=NULL, vec1=NULL,vec2=NULL,vec3=NULL,vec4=NULL,resultlist=list(), resultlistcounter=1) {
## functionality: calling function for qat_analyse_distribution_1d
## author: André Düsterhus
## date: 03.05.2010
## version: A0.1
## input: measurement_vector, workflowlist element, number of actual element, time vector (optional), latitude vector (optional), longitude vector (optional), 4 optional vectors, resultlist (optional), counter of resultlist (optional)
## output: list with the results and parameters of the distribution
	numofbars <- as.numeric(workflowlist_part$numofbars)
	if (mode(numofbars)=="list") {
		numofbars <- as.numeric(numofbars$value)
	}
	resultlist[[resultlistcounter <- resultlistcounter+1]] <- list(element=element, method='dist', result =qat_analyse_distribution_1d(measurement_vector, numofbars))
	return(resultlist)
}
