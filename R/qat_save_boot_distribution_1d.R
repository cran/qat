qat_save_boot_distribution_1d <-
function(resultlist_part, baseunit="") {
## functionality: save distribution
## author: André Düsterhus
## date: 12.04.2011
## version: A0.1
## input: resultlist part from qat_analyse_lim_rule_sigma_1d, optional: baseunit
## output: savelist
	method <- "dist_boot"
	meanings <- NULL
	longname <- list(first_moment="First-Moment-Vector of a Boot-Distribution Test", second_moment="Second-Moment-Vector of a Boot-Distribution Test", third_moment="Third-Moment-Vector of a Boot-Distribution Test", fourth_moment="Fourth-Moment-Vector of a Boot-Distribution Test", standard_deviation="Standard-Deviation-Vector of a Boot-Distribution Test", skewness="Skewness-Vector of a Boot-Distribution Test", kurtosis="Kurtosis-Vector of a Boot-Distribution Test", median="Median-Vector of a Boot-Distribution Test", p5_quantile="5% percentile-Vektor of a Boot-Distribution Test", p95_quantile="95% percentile-Vector of a Boot-Distribution Test", p25_quantile="25% percentile-Vector of a Boot-Distribution Test", p75_quantile="75% percentile-Vector of a Boot-Distribution Test")
	fillvalue <- -999
	if (baseunit !="") {
		unit <- list(first_moment=baseunit, second_moment=paste(baseunit,"^2", sep=""), third_moment=paste(baseunit,"^3", sep=""), fourth_moment=paste(baseunit,"^4", sep=""), standard_deviation=baseunit, skewness=baseunit, kurtosis=baseunit, median=baseunit, p5_quantile=baseunit, p95_quantile=baseunit, p25_quantile=baseunit, p75_quantile=baseunit)
	} else {
		unit <- rep ("unitless", 12)
	}
	bootruns <- resultlist_part$result$bootruns
	dimension <- list(bootdist_vec=bootruns, bootdist_vec=bootruns, bootdist_vec=bootruns, bootdist_vec=bootruns, bootdist_vec=bootruns, bootdist_vec=bootruns, bootdist_vec=bootruns, bootdist_vec=bootruns, bootdist_vec=bootruns, bootdist_vec=bootruns, bootdist_vec=bootruns, bootdist_vec=bootruns)
	parameter <- list(bootruns=resultlist_part$result$bootruns)
	picnames <- list(firstpic = "bootdist")
	content <-  list(first_moment=resultlist_part$result$stat$first_moment, second_moment=resultlist_part$result$stat$second_moment, third_moment=resultlist_part$result$stat$third_moment, fourth_moment=resultlist_part$result$stat$fourth_moment, standard_deviation=resultlist_part$result$stat$standard_deviation, skewness=resultlist_part$result$stat$skewness, kurtosis=resultlist_part$result$stat$kurtosis, median=resultlist_part$result$stat$median, p5_quantile=resultlist_part$result$stat$p5_quantile, p95_quantile=resultlist_part$result$stat$p95_quantile, p25_quantile=resultlist_part$result$stat$p25_quantile, p75_quantile=resultlist_part$result$stat$p75_quantile)
	savelist <- list(method = method, meanings = meanings, longname = longname, fillvalue = fillvalue, unit = unit, dimension = dimension, parameter = parameter, picnames=picnames, content = content)
	return(savelist)	
}
