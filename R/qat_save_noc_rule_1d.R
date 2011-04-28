qat_save_noc_rule_1d <-
function(resultlist_part, baseunit="") {
## functionality: save noc-rule
## author: André Düsterhus
## date: 11.04.2011
## version: A0.2
## input: resultlist part from qat_analyse_noc_rule_1d, optional: baseunit
## output: savelist
	method <- "noc"
	meanings <- list(flagvector = "0: Value is ok, 1: Repetition error")
	longname <- list(flagvector = "Flagvector of a NOC Test")
	fillvalue <- -999
	unit <- list(flagvector = "unitless")
	dimension <- list(mes_vec = NaN)
	parameter <- list(max_return_elements=resultlist_part$result$max_return_elements)
	picnames <- list(firstpic = "noc")
	content <-  list(flagvector=resultlist_part$result$flagvector)
	savelist <- list(method = method, meanings = meanings, longname = longname, fillvalue = fillvalue, unit = unit, dimension = dimension, parameter = parameter, picnames=picnames, content = content)
	return(savelist)	
}

