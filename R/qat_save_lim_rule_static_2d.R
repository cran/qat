qat_save_lim_rule_static_2d <-
function(resultlist_part, baseunit="") {
## functionality: save lim-static-rule
## author: André Düsterhus
## date: 22.10.2012
## version: A0.2
## input: resultlist part from qat_analyse_lim_rule_static_2d, optional: baseunit
## output: savelist
	method <- "lim_static"
	meanings <- list(flagvector = "Original value exceeds: 1 maxmimum value, -1 minimum value, 0 no value")
	longname <- list(flagvector = "Flagvector of a LIM-static Test")
	fillvalue <- -999
	unit <- list(flagvector = "unitless")
	dimension <- list(dim=list(mes_vec1 = NaN, mes_vec2 = NaN))
	parameter <- list(min_value=resultlist_part$result$min_value, max_value=resultlist_part$result$max_value)
	picnames <- list(firstpic = "lim_static")
	content <-  list(flagvector=resultlist_part$result$flagvector)
	savelist <- list(method = method, meanings = meanings, longname = longname, fillvalue = fillvalue, unit = unit, dimension = dimension, parameter = parameter, picnames=picnames, content = content)
	return(savelist)	
}
