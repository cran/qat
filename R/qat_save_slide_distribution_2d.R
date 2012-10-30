qat_save_slide_distribution_2d <-
function(resultlist_part, baseunit="") {
## functionality: save distribution
## author: André Düsterhus
## date: 23.10.2012
## version: A0.1
## input: resultlist part from qat_analyse_slide_distribution_2d, optional: baseunit
## output: savelist
	method <- "dist_slide"
	meanings <- NULL
	longname <- list(first_moment="First-Moment-Vector of a Slide-Distribution Test", second_moment="Second-Moment-Vector of a Slide-Distribution Test", third_moment="Third-Moment-Vector of a Slide-Distribution Test", fourth_moment="Fourth-Moment-Vector of a Slide-Distribution Test", standard_deviation="Standard-Deviation-Vector of a Slide-Distribution Test", skewness="Skewness-Vector of a Slide-Distribution Test", kurtosis="Kurtosis-Vector of a Slide-Distribution Test", median="Median-Vector of a Slide-Distribution Test", p5_quantile="5% percentile-Vektor of a Slide-Distribution Test", p95_quantile="95% percentile-Vector of a Slide-Distribution Test", p25_quantile="25% percentile-Vector of a Slide-Distribution Test", p75_quantile="75% percentile-Vector of a Slide-Distribution Test")
	fillvalue <- -999
	if (baseunit !="") {
		unit <- list(first_moment=baseunit, second_moment=paste(baseunit,"^2", sep=""), third_moment=paste(baseunit,"^3", sep=""), fourth_moment=paste(baseunit,"^4", sep=""), standard_deviation=baseunit, skewness=baseunit, kurtosis=baseunit, median=baseunit, p5_quantile=baseunit, p95_quantile=baseunit, p25_quantile=baseunit, p75_quantile=baseunit)
	} else {
		unit <- rep ("unitless", 12)
	}
	blocknum <- dim(resultlist_part$result$stat$first_moment)[1]
	dimension <- list(first_moment=list(slidedist_vec=blocknum, mes_vec2=NaN), second_moment=list(slidedist_vec=blocknum, mes_vec2=NaN), third_moment=list(slidedist_vec=blocknum, mes_vec2=NaN), fourth_moment=list(slidedist_vec=blocknum, mes_vec2=NaN), standard_deviation=list(slidedist_vec=blocknum, mes_vec2=NaN), skewness=list(slidedist_vec=blocknum, mes_vec2=NaN), kurtosis=list(slidedist_vec=blocknum, mes_vec2=NaN), median=list(slidedist_vec=blocknum, mes_vec2=NaN), p5_quantile=list(slidedist_vec=blocknum, mes_vec2=NaN), p95_quantile=list(slidedist_vec=blocknum, mes_vec2=NaN),p25_quantile=list(slidedist_vec=blocknum, mes_vec2=NaN), p75_quantile=list(slidedist_vec=blocknum, mes_vec2=NaN))
	parameter <- list(blocksize=resultlist_part$result$blocksize)
	picnames <- list(firstpic = "slidedist_1", secondpic = "slidedist_2", thridpic = "slidedist_3")
	content <-  list(first_moment=resultlist_part$result$stat$first_moment, second_moment=resultlist_part$result$stat$second_moment, third_moment=resultlist_part$result$stat$third_moment, fourth_moment=resultlist_part$result$stat$fourth_moment, standard_deviation=resultlist_part$result$stat$standard_deviation, skewness=resultlist_part$result$stat$skewness, kurtosis=resultlist_part$result$stat$kurtosis, median=resultlist_part$result$stat$median, p5_quantile=resultlist_part$result$stat$p5_quantile, p95_quantile=resultlist_part$result$stat$p95_quantile, p25_quantile=resultlist_part$result$stat$p25_quantile, p75_quantile=resultlist_part$result$stat$p75_quantile)
	savelist <- list(method = method, meanings = meanings, longname = longname, fillvalue = fillvalue, unit = unit, dimension = dimension, parameter = parameter, picnames=picnames, content = content)
	return(savelist)	
}
