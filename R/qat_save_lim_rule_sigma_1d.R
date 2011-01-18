qat_save_lim_rule_sigma_1d <-
function(resultlist_part, baseunit="") {
## functionality: save lim-sigma-rule
## author: André Düsterhus
## date: 04.04.2011
## version: A0.2
## input: resultlist part from qat_analyse_lim_rule_sigma_1d, optional: baseunit
## output: savelist
	method <- "lim_sigma"
	meanings <- list(flagvector = "Original value exceeds: 1 maxmimum value, -1 minimum value, 0 no value")
	longname <- list(flagvector = "Flagvector of a LIM-sigma Test")
	fillvalue <- -999
	unit <- list(flagvector = "unitless")
	dimension <- list(mes_vec = NaN)
	parameter <- list(sigma_factor=resultlist_part$result$sigma_factor, meanofvector = resultlist_part$result$meanofvector , sdofvector = resultlist_part$result$sdofvector)
	picnames <- list(firstpic = "lim_sigma")
	content <-  list(flagvector=resultlist_part$result$flagvector)
	savelist <- list(method = method, meanings = meanings, longname = longname, fillvalue = fillvalue, unit = unit, dimension = dimension, parameter = parameter, picnames=picnames, content = content)
	return(savelist)	
}
