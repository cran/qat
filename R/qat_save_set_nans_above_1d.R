qat_save_set_nans_above_1d <-
function(resultlist_part, baseunit="") {
## functionality: save set nan above
## author: André Düsterhus
## date: 21.04.2011
## version: A0.1
## input: resultlist part from qat_analyse_set_nans_above_1d, optional: baseunit
## output: savelist
	method <- "nan_above"
	returntext <- paste("setting values above",resultlist_part$result$nan_above,"to NaN")
	return(list(method = method, returntext=returntext))	
}

