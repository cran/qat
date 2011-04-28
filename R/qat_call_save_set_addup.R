qat_call_save_set_addup <-
function(resultlist_part, element = -999, time = NULL, height = NULL, lat = NULL, lon = NULL, vec1 = NULL, vec2 = NULL, vec3 = NULL, vec4 = NULL, baseunit = NULL, savelist = list(), savelistcounter = 1) {
## functionality: calling save function for qat_save_set_addup_1d
## author: André Düsterhus
## date: 21.04.2011
## version: A0.1
## input: measurement_vector, workflowlist element, number of actual element, time vector (optional), latitude vector (optional), longitude vector (optional), 4 optional vectors, resultlist (optional), counter of resultlist (optional)
## output: list with the results and parameters of the lim rule analysis
	# add informations to savelist
	savelist[[savelistcounter <- savelistcounter+1]] <- list(element=element, tosave = qat_save_set_addup_1d(resultlist_part, baseunit=""))
	return(savelist)
}

