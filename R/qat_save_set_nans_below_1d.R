qat_save_set_nans_below_1d <-
function(resultlist_part, baseunit="") {
## functionality: save set nan below
## author: André Düsterhus
## date: 21.04.2011
## version: A0.1
## input: resultlist part from qat_analyse_set_nans_below_1d, optional: baseunit
## output: savelist
	method <- "nan_below"
	returntext <- paste("setting values below",resultlist_part$result$nan_below,"to NaN")
	return(list(method = method, returntext=returntext))	
}
