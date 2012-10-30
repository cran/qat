qat_call_noc_rule <-
function(measurement_vector, workflowlist_part, element=-999, time=NULL, height= NULL, lat=NULL, lon=NULL, vec1=NULL, vec2=NULL, vec3=NULL, vec4=NULL, resultlist=list(), resultlistcounter=1) {
## functionality: calling function for qat_analyse_noc_rule
## author: André Düsterhus
## date: 09.03.2010
## version: A0.1
## input: measurement_vector, workflowlist element, number of actual element, time vector (optional), latitude vector (optional), longitude vector (optional), 4 optional vectors, resultlist (optional), counter of resultlist (optional)
## output: list with the result and parameter of the noc rule analysis
	if(!is.null(workflowlist_part$max_return_elements)) {
		return_elements <- as.numeric(workflowlist_part$max_return_elements)
		if (mode(return_elements)=="list") {
			return_elements <- as.numeric(return_elements$value)
		}
		if (is.null(dim(measurement_vector))) {
			resultlist[[resultlistcounter <- resultlistcounter+1]] <- list(element=element, method='noc', result=qat_analyse_noc_rule_1d(measurement_vector, return_elements))
		}
		if (length(dim(measurement_vector))==2) {
			resultlist[[resultlistcounter <- resultlistcounter+1]] <- list(element=element, method='noc', result=qat_analyse_noc_rule_2d(measurement_vector, return_elements))
		}
	}
	return(resultlist)
}
