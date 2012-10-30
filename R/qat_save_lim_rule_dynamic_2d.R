qat_save_lim_rule_dynamic_2d <-
function(resultlist_part, baseunit="") {
## functionality: save lim-dynamic-rule
## author: André Düsterhus
## date: 22.10.2012
## version: A0.2
## input: resultlist part from qat_analyse_lim_rule_dynamic_2d, optional: baseunit
## output: savelist
	method <- "lim_dynamic"
	meanings <- list(flagvector = "Original value exceeds: 1 maxmimum value, -1 minimum value, 0 no value")
	longname <- list(flagvector = "Flagvector of a LIM-dynamic Test")
	fillvalue <- -999
	unit <- list(flagvector = "unitless")
	dimension <- list(dim=list(mes_vec1 = NaN, mes_vec2 = NaN))
	if (!is.null(resultlist_part$result$max_vector_identifier)) {
		id_max <- paste("add",resultlist_part$result$max_vector_identifier,sep="")
	} else {
		id_max <- NULL
	}
	if (!is.null(resultlist_part$result$min_vector_identifier)) {
		id_min <- paste("add",resultlist_part$result$min_vector_identifier,sep="")
	} else {
		id_min <- NULL
	}
	parameter <- list(min_vector_name=resultlist_part$result$min_vector_name, max_vector_name=resultlist_part$result$max_vector_name, min_vector_identifier=id_min, max_vector_identifier=id_max)
	picnames <- list(firstpic = "lim_dynamic")
	content <-  list(flagvector=resultlist_part$result$flagvector)
	savelist <- list(method = method, meanings = meanings, longname = longname, fillvalue = fillvalue, unit = unit, dimension = dimension, parameter = parameter, picnames=picnames, content = content)
	return(savelist)	
}
