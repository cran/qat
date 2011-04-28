qat_save_trimmed_distribution_1d <-
function(resultlist_part, baseunit="") {
## functionality: save distribution
## author: André Düsterhus
## date: 12.04.2011
## version: A0.1
## input: resultlist part from qat_analyse_lim_rule_sigma_1d, optional: baseunit
## output: savelist
	method <- "dist_trimmed"
	meanings <- NULL
	longname <- list(first_moment="First-Moment-Vector of a Trimmed-Distribution Test", second_moment="Second-Moment-Vector of a Trimmed-Distribution Test", third_moment="Third-Moment-Vector of a Trimmed-Distribution Test", fourth_moment="Fourth-Moment-Vector of a Trimmed-Distribution Test", standard_deviation="Standard-Deviation-Vector of a Trimmed-Distribution Test", skewness="Skewness-Vector of a Trimmed-Distribution Test", kurtosis="Kurtosis-Vector of a Trimmed-Distribution Test")
	fillvalue <- -999
	if (baseunit !="") {
		unit <- list(first_moment=baseunit, second_moment=paste(baseunit,"^2", sep=""), third_moment=paste(baseunit,"^3", sep=""), fourth_moment=paste(baseunit,"^4", sep=""), standard_deviation=baseunit, skewness=baseunit, kurtosis=baseunit)
	} else {
		unit <- rep ("unitless", 7)
	}
	percentilenum <- length(resultlist_part$result$stat$first_moment)
	dimension <- list(trimmed_vec=percentilenum, trimmed_vec=percentilenum, trimmed_vec=percentilenum, trimmed_vec=percentilenum, trimmed_vec=percentilenum, trimmed_vec=percentilenum, trimmed_vec=percentilenum)
	parameter <- NULL
	picnames <- list(firstpic = "trimmeddist")
	content <-  list(first_moment=resultlist_part$result$stat$first_moment, second_moment=resultlist_part$result$stat$second_moment, third_moment=resultlist_part$result$stat$third_moment, fourth_moment=resultlist_part$result$stat$fourth_moment, standard_deviation=resultlist_part$result$stat$standard_deviation, skewness=resultlist_part$result$stat$skewness, kurtosis=resultlist_part$result$stat$kurtosis)
	savelist <- list(method = method, meanings = meanings, longname = longname, fillvalue = fillvalue, unit = unit, dimension = dimension, parameter = parameter, picnames=picnames, content = content)
	return(savelist)	
}

