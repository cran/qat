qat_call_save_lim_rule <-
function(resultlist_part, element = -999, time = NULL, height = NULL, lat = NULL, lon = NULL, vec1 = NULL, vec2 = NULL, vec3 = NULL, vec4 = NULL, baseunit = NULL, savelist = list(), savelistcounter = 1) {
## functionality: calling function for qat_save_lim_rule_static, qat_save_lim_rule_sigma and qat_save_lim_rule_dynamic
## author: André Düsterhus
## date: 04.04.2011
## version: A0.2
## input: measurement_vector, workflowlist element, number of actual element, time vector (optional), latitude vector (optional), longitude vector (optional), 4 optional vectors, resultlist (optional), counter of resultlist (optional)
## output: list with the results and parameters of the lim rule analysis
	# add informations to savelist
	if (resultlist_part$method == 'lim_static') {
		savelist[[savelistcounter <- savelistcounter+1]] <- list(element=element, tosave = qat_save_lim_rule_static_1d(resultlist_part, baseunit=""))
	}
	if (resultlist_part$method == 'lim_sigma') {
		savelist[[savelistcounter <- savelistcounter+1]] <- list(element=element, tosave = qat_save_lim_rule_sigma_1d(resultlist_part, baseunit=""))
	}
	if (resultlist_part$method == 'lim_dynamic') {
		savelist[[savelistcounter <- savelistcounter+1]] <- list(element=element, tosave = qat_save_lim_rule_dynamic_1d(resultlist_part, baseunit=""))
	}
	return(savelist)
}

