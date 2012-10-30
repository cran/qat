qat_call_save_noc_rule <-
function(resultlist_part, element = -999, dim_mv=1, time = NULL, height = NULL, lat = NULL, lon = NULL, vec1 = NULL, vec2 = NULL, vec3 = NULL, vec4 = NULL, baseunit = NULL, savelist = list(), savelistcounter = 1) {
## functionality: calling function for qat_save_noc_rule
## author: André Düsterhus
## date: 11.04.2011
## version: A0.2
## input: measurement_vector, workflowlist element, number of actual element, time vector (optional), latitude vector (optional), longitude vector (optional), 4 optional vectors, resultlist (optional), counter of resultlist (optional)
## output: list with the results and parameters of the noc rule analysis
	# add informations to savelist
	if (resultlist_part$method == 'noc') {
		if (dim_mv==1) {
			savelist[[savelistcounter <- savelistcounter+1]] <- list(element=element, tosave = qat_save_noc_rule_1d(resultlist_part, baseunit=""))
		}
		if (dim_mv==2) {
			savelist[[savelistcounter <- savelistcounter+1]] <- list(element=element, tosave = qat_save_noc_rule_2d(resultlist_part, baseunit=""))			
		}

	}
	return(savelist)
}
