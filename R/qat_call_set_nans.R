qat_call_set_nans <-
function(measurement_vector, workflowlist_part, element=-999, time=NULL, height= NULL, lat=NULL, lon=NULL, vec1=NULL,vec2=NULL,vec3=NULL,vec4=NULL,resultlist=list(), resultlistcounter=1) {
## functionality: calling function for qat_analyse_set_nans, qat_analyse_set_nans_above, qat_analyse_set_nans_below
## author: André Düsterhus
## date: 09.03.2010
## version: A0.1
## input: measurement_vector, workflowlist element, number of actual element, time vector (optional), latitude vector (optional), longitude vector (optional), 4 optional vectors, resultlist (optional), counter of resultlist (optional)
## output: corrected measurement_vector
	if(!is.null(workflowlist_part$nan_value)) {
		nan_value <- as.numeric(workflowlist_part$nan_value)
		if (mode(nan_value)=="list") {
			nan_value <- as.numeric(nan_value$value)
		}
		if (is.null(dim(measurement_vector))) {
			resultlist[[resultlistcounter <- resultlistcounter+1]] <- list(element=element, method='nan_value', result =qat_analyse_set_nans_1d(measurement_vector, nan_value))
		}
		if (length(dim(measurement_vector))==2) {
			resultlist[[resultlistcounter <- resultlistcounter+1]] <- list(element=element, method='nan_value', result =qat_analyse_set_nans_1d(measurement_vector, nan_value))
		}		
	}
	if(!is.null(workflowlist_part$nan_above)) {
		nan_above <- as.numeric(workflowlist_part$nan_above)
		if (mode(nan_above)=="list") {
			nan_above <- as.numeric(nan_above$value)
		}
		if (is.null(dim(measurement_vector))) {
			resultlist[[resultlistcounter <- resultlistcounter+1]] <- list(element=element, method='nan_above', result =qat_analyse_set_nans_above_1d(measurement_vector, nan_above))
		}
		if (length(dim(measurement_vector))==2) {
			resultlist[[resultlistcounter <- resultlistcounter+1]] <- list(element=element, method='nan_above', result =qat_analyse_set_nans_above_1d(measurement_vector, nan_above))
		}
	}
	if(!is.null(workflowlist_part$nan_below)) {
		nan_below <- as.numeric(workflowlist_part$nan_below)
		if (mode(nan_below)=="list") {
			nan_below <- as.numeric(nan_below$value)
		}
		if (is.null(dim(measurement_vector))) {
			resultlist[[resultlistcounter <- resultlistcounter+1]] <- list(element=element, method='nan_below', result =qat_analyse_set_nans_below_1d(measurement_vector, nan_below))
		}
		if (length(dim(measurement_vector))==2) {
			resultlist[[resultlistcounter <- resultlistcounter+1]] <- list(element=element, method='nan_below', result =qat_analyse_set_nans_below_2d(measurement_vector, nan_below))
		}
	}
	return(resultlist)
}
