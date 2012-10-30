qat_call_set_mean <-
function(measurement_vector, workflowlist_part, element=-999, time=NULL, height= NULL, lat=NULL, lon=NULL, vec1=NULL,vec2=NULL,vec3=NULL,vec4=NULL,resultlist=list(), resultlistcounter=1) {
## functionality: calling function for qat_analyse_set_mean
## author: André Düsterhus
## date: 19.04.2010
## version: A0.1
## input: measurement_vector, workflowlist element, number of actual element, time vector (optional), latitude vector (optional), longitude vector (optional), 4 optional vectors, resultlist (optional), counter of resultlist (optional)
## output: corrected measurement_vector
	if(!is.null(workflowlist_part$blocksize)) {
		blocksize_value <- as.numeric(workflowlist_part$blocksize)
		if (mode(blocksize_value)=="list") {
			blocksize_value <- as.numeric(blocksize_value$value)
		}
		if (is.null(dim(measurement_vector))) {
			resultlist[[resultlistcounter <- resultlistcounter+1]] <- list(element=element, method='mean', result =qat_analyse_set_mean_1d(measurement_vector, blocksize_value))
		}
		if (length(dim(measurement_vector))==2) {
			resultlist[[resultlistcounter <- resultlistcounter+1]] <- list(element=element, method='mean', result =qat_analyse_set_mean_2d(measurement_vector, blocksize_value))
		}		
	}
	return(resultlist)
}
