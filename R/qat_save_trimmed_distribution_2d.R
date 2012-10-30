qat_save_trimmed_distribution_2d <-
function(resultlist_part, baseunit="") {
## functionality: save distribution
## author: André Düsterhus
## date: 23.10.2012
## version: A0.1
## input: resultlist part from qat_analyse_trimmed_distribution_2d, optional: baseunit
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
	percentilenum <- dim(resultlist_part$result$stat$first_moment)[1]
	dimension <- list(first_moment=list(trimmed_vec=percentilenum, mes_vec2=NaN), second_moment=list(trimmed_vec=percentilenum, mes_vec2=NaN), third_moment=list(trimmed_vec=percentilenum, mes_vec2=NaN), fourth_moment=list(trimmed_vec=percentilenum, mes_vec2=NaN), standard_deviation=list(trimmed_vec=percentilenum, mes_vec2=NaN), skewness=list(trimmed_vec=percentilenum, mes_vec2=NaN), kurtosis=list(trimmed_vec=percentilenum, mes_vec2=NaN))
	parameter <- NULL
	picnames <- list(firstpic = "trimmeddist")
	content <-  list(first_moment=resultlist_part$result$stat$first_moment, second_moment=resultlist_part$result$stat$second_moment, third_moment=resultlist_part$result$stat$third_moment, fourth_moment=resultlist_part$result$stat$fourth_moment, standard_deviation=resultlist_part$result$stat$standard_deviation, skewness=resultlist_part$result$stat$skewness, kurtosis=resultlist_part$result$stat$kurtosis)
	savelist <- list(method = method, meanings = meanings, longname = longname, fillvalue = fillvalue, unit = unit, dimension = dimension, parameter = parameter, picnames=picnames, content = content)
	return(savelist)	
}
