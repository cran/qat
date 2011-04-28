qat_save_roc_rule_static_1d <-
function(resultlist_part, baseunit="") {
## functionality: save roc-static-rule
## author: André Düsterhus
## date: 11.04.2011
## version: A0.2
## input: resultlist part from qat_analyse_roc_rule_static_1d, optional: baseunit
## output: savelist
	method <- "roc_static"
	meanings <- list(flagvector = "1: upward error, -1: downward error, 0: is ok")
	longname <- list(flagvector = "Flagvector of a ROC-static Test")
	fillvalue <- -999
	unit <- list(flagvector = "unitless")
	dimension <- list(mes_vec = NaN)
	parameter <- list(max_downward_value=resultlist_part$result$max_downward_value, max_upward_value=resultlist_part$result$max_upward_value)
	picnames <- list(firstpic = "roc_static")
	content <-  list(flagvector=resultlist_part$result$flagvector)
	savelist <- list(method = method, meanings = meanings, longname = longname, fillvalue = fillvalue, unit = unit, dimension = dimension, parameter = parameter, picnames=picnames, content = content)
	return(savelist)	
}

