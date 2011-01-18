qat_save_distribution_1d <-
function(resultlist_part, baseunit="") {
## functionality: save distribution
## author: André Düsterhus
## date: 12.04.2011
## version: A0.1
## input: resultlist part from qat_analyse_lim_rule_sigma_1d, optional: baseunit
## output: savelist
	method <- "dist"
	meanings <- list(breaks = "Border values of the bars", counts= "Elements of original value at a bar between the border values")
	longname <- list(breaks = "Breakvector of a Distribution Test", counts= "Countsvector of a Distribution Test")
	fillvalue <- -999
	unit <- list(breaks = "unitless", counts = "unitless")
	dimension <- list(break_vec = resultlist_part$result$numofbars+1, counts_vec = resultlist_part$result$numofbars)
	parameter <- list(numofbars=resultlist_part$result$numofbars, first_moment=resultlist_part$result$stat$first_moment, second_moment=resultlist_part$result$stat$second_moment, third_moment=resultlist_part$result$stat$third_moment, fourth_moment=resultlist_part$result$stat$fourth_moment, standard_deviation=resultlist_part$result$stat$standard_deviation, skewness=resultlist_part$result$stat$skewness, kurtosis=resultlist_part$result$stat$kurtosis, median=resultlist_part$result$stat$median, p5_quantile=resultlist_part$result$stat$p5_quantile, p95_quantile=resultlist_part$result$stat$p95_quantile, p25_quantile=resultlist_part$result$stat$p25_quantile, p75_quantile=resultlist_part$result$stat$p75_quantile)
	picnames <- list(firstpic = "dist")
	content <-  list(breaks=resultlist_part$result$hist$breaks, counts=resultlist_part$result$hist$counts)
	savelist <- list(method = method, meanings = meanings, longname = longname, fillvalue = fillvalue, unit = unit, dimension = dimension, parameter = parameter, picnames=picnames, content = content)
	return(savelist)	
}
