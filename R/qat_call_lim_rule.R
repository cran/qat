qat_call_lim_rule <-
function(measurement_vector, workflowlist_part, element=-999, time=NULL, height= NULL, lat=NULL, lon=NULL, vec1=NULL,vec2=NULL,vec3=NULL,vec4=NULL,resultlist=list(), resultlistcounter=1) {
## functionality: calling function for qat_analyse_lim_rule_static, qat_analyse_lim_rule_sigma and qat_analyse_lim_rule_dynamic
## author: André Düsterhus
## date: 09.03.2010
## version: A0.1
## input: measurement_vector, workflowlist element, number of actual element, time vector (optional), latitude vector (optional), longitude vector (optional), 4 optional vectors, resultlist (optional), counter of resultlist (optional)
## output: list with the results and parameters of the lim rule analysis
	# check if dynamic lim-rule exists
	if(!is.null(workflowlist_part$minimum_vector) || !is.null(workflowlist_part$maximum_vector)) {
		# check if minimum-vector for dynamic lim-rule exists
		if (is.null(workflowlist_part$minimum_vector)) {
			# minimum vector does not exist and so unreachable values will be added
			min_vec <- measurement_vector - 1
		} else {
			# set minimum vector with one additional vector, which is specified by minimum_vector-tag.
			# have to be enhanced, if more additional vectors will be set
			if(workflowlist_part$minimum_vector=='vec1') {
				min_vec <- vec1
			}
			if(workflowlist_part$minimum_vector=='vec2') {
				min_vec <- vec2
			}
			if(workflowlist_part$minimum_vector=='vec3') {
				min_vec <- vec3
			} 
			if(workflowlist_part$minimum_vector=='vec4') {
				min_vec <- vec4
			} 
		}
		# check if maximum-vector for dynamic lim-rule exists
		if (is.null(workflowlist_part$maximum_vector)) {
			# maximum vector does not exist and so unreachable values will be added
			max_vec <- measurement_vector + 1
		} else {
			# set maximum vector with one additional vector, which is specified by maximum_vector-tag.
			# have to be enhanced, if more additional vectors will be set
			if(workflowlist_part$maximum_vector=='vec1') {
				max_vec <- vec1
			}
			if(workflowlist_part$maximum_vector=='vec2') {
				max_vec <- vec2
			}
			if(workflowlist_part$maximum_vector=='vec3') {
				max_vec <- vec3
			} 
			if(workflowlist_part$maximum_vector=='vec4') {
				max_vec <- vec4
			} 
		}
		# add informations to resultlist
		if (is.null(dim(measurement_vector))) {
			resultlist[[resultlistcounter <- resultlistcounter+1]] <- list(element=element, method='lim_dynamic', result =qat_analyse_lim_rule_dynamic_1d(measurement_vector, min_vec, max_vec,workflowlist_part$minimum_vector_name, workflowlist_part$maximum_vector_name, workflowlist_part$minimum_vector, workflowlist_part$maximum_vector))
		}
		if (length(dim(measurement_vector))==2) {
			resultlist[[resultlistcounter <- resultlistcounter+1]] <- list(element=element, method='lim_dynamic', result =qat_analyse_lim_rule_dynamic_2d(measurement_vector, min_vec, max_vec,workflowlist_part$minimum_vector_name, workflowlist_part$maximum_vector_name, workflowlist_part$minimum_vector, workflowlist_part$maximum_vector))
		}		
	} #lim dynamic
	if(!is.null(workflowlist_part$minimum_value) || !is.null(workflowlist_part$maximum_value)) {
		# check if minimum-value for static lim-rule exists
		if (is.null(workflowlist_part$minimum_value)) {
			# minimum value does not exist and so it will be set on an unreachable value
			min_val <- min(measurement_vector) - 1
		} else {
			min_val <- as.numeric(workflowlist_part$minimum_value)
		}
		# check if maximum-value for static lim-rule exists
		if (is.null(workflowlist_part$maximum_value)) {
			# maximum value does not exist and so it will be set on an unreachable value
			max_val <- max(measurement_vector) + 1
		} else {
			# set maximum value
			max_val <- as.numeric(workflowlist_part$maximum_value)
		}
		if (mode(min_val)=="list") {
			min_val <- as.numeric(min_val$value)
		}
		if (mode(max_val)=="list") {
			max_val <- as.numeric(max_val$value)
		}
		# add informations to resultlist
		if (is.null(dim(measurement_vector))) {
			resultlist[[resultlistcounter <- resultlistcounter+1]] <- list(element=element, method='lim_static', result=qat_analyse_lim_rule_static_1d(measurement_vector, min_val, max_val))
		}
		if (length(dim(measurement_vector))==2) {
			resultlist[[resultlistcounter <- resultlistcounter+1]] <- list(element=element, method='lim_static', result=qat_analyse_lim_rule_static_2d(measurement_vector, min_val, max_val))
		}	
	} #lim static
	if(!is.null(workflowlist_part$sigma_factor)) {
		# check if sigma factor for lim-rule exists
		sigma_factor <- as.numeric(workflowlist_part$sigma_factor)
		if (mode(sigma_factor)=="list") {
			sigma_factor <- as.numeric(as.character(workflowlist_part$sigma_factor)[6])
		}
		# add informations to resultlist
		if (is.null(dim(measurement_vector))) {
			resultlist[[resultlistcounter <- resultlistcounter+1]] <- list(element=element, method='lim_sigma', result=qat_analyse_lim_rule_sigma_1d(measurement_vector,sigma_factor))
		}
		if (length(dim(measurement_vector)) ==2) {
			resultlist[[resultlistcounter <- resultlistcounter+1]] <- list(element=element, method='lim_sigma', result=qat_analyse_lim_rule_sigma_2d(measurement_vector,sigma_factor))
		}		
	} #lim sigmafactor
	return(resultlist)
}
