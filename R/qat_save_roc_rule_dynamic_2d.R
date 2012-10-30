qat_save_roc_rule_dynamic_2d <-
function(resultlist_part, baseunit="") {
## functionality: save roc-dynamic-rule
## author: André Düsterhus
## date: 22.10.2012
## version: A0.2
## input: resultlist part from qat_analyse_roc_rule_dynamic_2d, optional: baseunit
## output: savelist
	method <- "roc_dynamic"
	meanings <- list(flagvector = "1: upward error, -1: downward error, 0: is ok")
	longname <- list(flagvector = "Flagvector of a ROC-dynamic Test")
	fillvalue <- -999
	unit <- list(flagvector = "unitless")
	dimension <- list(dim=list(mes_vec1 = NaN, mes_vec2 = NaN))
	if (!is.null(resultlist_part$result$downward_vector_identifier)) {
		id_down <- paste("add",resultlist_part$result$downward_vector_identifier,sep="")
	} else {
		id_down <- NULL
	}
	if (!is.null(resultlist_part$result$upward_vector_identifier)) {
		id_up <- paste("add",resultlist_part$result$upward_vector_identifier,sep="")
	} else {
		id_up <- NULL
	}
	parameter <- list(downward_vector_name=resultlist_part$result$downward_vector_name, upward_vector_name=resultlist_part$result$upward_vector_name, downward_vector_identifier=id_down, upward_vector_identifier=id_up)
	picnames <- list(firstpic = "roc_dynamic")
	content <-  list(flagvector=resultlist_part$result$flagvector)
	savelist <- list(method = method, meanings = meanings, longname = longname, fillvalue = fillvalue, unit = unit, dimension = dimension, parameter = parameter, picnames=picnames, content = content)
	return(savelist)	
}
