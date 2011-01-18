qat_call_save_set_nans <-
function(resultlist_part, element = -999, time = NULL, height = NULL, lat = NULL, lon = NULL, vec1 = NULL, vec2 = NULL, vec3 = NULL, vec4 = NULL, baseunit = NULL, savelist = list(), savelistcounter = 1) {
## functionality: calling save function for qat_save_set_nans_1d, qat_save_set_nans_above_1d and qat_save_set_nans_below_1d
## author: André Düsterhus
## date: 21.04.2011
## version: A0.1
## input: measurement_vector, workflowlist element, number of actual element, time vector (optional), latitude vector (optional), longitude vector (optional), 4 optional vectors, resultlist (optional), counter of resultlist (optional)
## output: list with the results and parameters of the lim rule analysis
	# add informations to savelist
	if (resultlist_part$method == 'nan_value') {
		savelist[[savelistcounter <- savelistcounter+1]] <- list(element=element, tosave = qat_save_set_nans_1d(resultlist_part, baseunit=""))
	}
	if (resultlist_part$method == 'nan_above') {
		savelist[[savelistcounter <- savelistcounter+1]] <- list(element=element, tosave = qat_save_set_nans_above_1d(resultlist_part, baseunit=""))
	}
	if (resultlist_part$method == 'nan_below') {
		savelist[[savelistcounter <- savelistcounter+1]] <- list(element=element, tosave = qat_save_set_nans_below_1d(resultlist_part, baseunit=""))
	}
	return(savelist)
}
