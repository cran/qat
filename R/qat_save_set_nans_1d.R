qat_save_set_nans_1d <-
function(resultlist_part, baseunit="") {
## functionality: save set nan value
## author: André Düsterhus
## date: 21.04.2011
## version: A0.1
## input: resultlist part from qat_analyse_set_nans_1d, optional: baseunit
## output: savelist
	method <- "nan_value"
	returntext <- paste("setting values equal",resultlist_part$result$nan_value,"to NaN")
	return(list(method = method, returntext=returntext))	
}

