qat_save_set_addup_1d <-
function(resultlist_part, baseunit="") {
## functionality: save set addup
## author: André Düsterhus
## date: 21.04.2011
## version: A0.1
## input: resultlist part from qat_analyse_set_addup_1d, optional: baseunit
## output: savelist
	method <- "addup"
	returntext <- paste("adding up values with a blocksize of",resultlist_part$result$blocksize)
	return(list(method = method, returntext=returntext))	
}

