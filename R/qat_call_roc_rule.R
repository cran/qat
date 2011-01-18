qat_call_roc_rule <-
function(measurement_vector, workflowlist_part, element=-999, time=NULL, height= NULL, lat=NULL, lon=NULL, vec1=NULL, vec2=NULL, vec3=NULL, vec4=NULL, resultlist=list(), resultlistcounter=1) {
## functionality: calling function for qat_analyse_roc_rule_static and qat_analyse_roc_rule_dynamic
## author: André Düsterhus
## date: 09.03.2010
## version: A0.1
## input: measurement_vector, workflowlist element, number of actual element, time vector (optional), latitude vector (optional), longitude vector (optional), 4 optional vectors, resultlist (optional), counter of resultlist (optional)
## output: list with the results and parameters of the roc rule analysis
	# check if dynamic roc-rule exists
	if(!is.null(workflowlist_part$downward_vector) || !is.null(workflowlist_part$upward_vector)) {
		# check if minimum-vector for dynamic lim-rule exists
		if (is.null(workflowlist_part$downward_vector)) {
			# minimum vector does not exist and so unreachable values will be added
			down_vec <- array(Inf,measurement_vector)
		} else {
			# set minimum vector with one additional vector, which is specified by minimum_vector-tag.
			# have to be enhanced, if more additional vectors will be set
			if(workflowlist_part$downward_vector=='vec1') {
				down_vec <- vec1
			}
			if(workflowlist_part$downward_vector=='vec2') {
				down_vec <- vec2
			}
			if(workflowlist_part$downward_vector=='vec3') {
				down_vec <- vec3
			} 
			if(workflowlist_part$downward_vector=='vec4') {
				down_vec <- vec4
			} 
		}
		# check if maximum-vector for dynamic lim-rule exists
		if (is.null(workflowlist_part$upward_vector)) {
			# maximum vector does not exist and so unreachable values will be added
			up_vec <- array(Inf,measurement_vector)
		} else {
			# set maximum vector with one additional vector, which is specified by minimum_vector-tag.
			# have to be enhanced, if more additional vectors will be set
			if(workflowlist_part$upward_vector=='vec1') {
				up_vec <- vec1
			}
			if(workflowlist_part$upward_vector=='vec2') {
				up_vec <- vec2
			}
			if(workflowlist_part$upward_vector=='vec3') {
				up_vec <- vec3
			} 
			if(workflowlist_part$upward_vector=='vec4') {
				up_vec <- vec4
			} 
		}
		# add informations to resultlist
		resultlist[[resultlistcounter <- resultlistcounter+1]] <- list(element=element, method='roc_dynamic', result=qat_analyse_roc_rule_dynamic_1d(measurement_vector, up_vec, down_vec, workflowlist_part$upward_vector_name, workflowlist_part$downward_vector_name, workflowlist_part$upward_vector, workflowlist_part$downward_vector))				
	} #roc dynamic
	if(!is.null(workflowlist_part$downward_value) || !is.null(workflowlist_part$upward_value)) {
		# check if minimum-value for static lim-rule exists
		if (is.null(workflowlist_part$downward_value)) {
			# minimum value does not exist and so it will be set on an unreachable value
			down_val <- Inf
		} else {
			down_val <- as.numeric(workflowlist_part$downward_value)
		}
		# check if maximum-value for static lim-rule exists
		if (is.null(workflowlist_part$upward_value)) {
			# maximum value does not exist and so it will be set on an unreachable value
			up_val <- Inf
		} else {
			# set maximum value
			up_val <- as.numeric(workflowlist_part$upward_value)
		}
		if (mode(down_val)=="list") {
			down_val <- as.numeric(down_val$value)
		}
		if (mode(up_val)=="list") {
			up_val <- as.numeric(up_val$value)
		}
		# add informations to resultlist
		resultlist[[resultlistcounter <- resultlistcounter+1]] <- list(element=element, method='roc_static', result=qat_analyse_roc_rule_static_1d(measurement_vector, up_val, down_val))	
	} #roc static
	return(resultlist)
}
