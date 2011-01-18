qat_call_plot_boot_distribution <-
function(resultlist_part, measurement_vector=NULL, time=NULL, height= NULL, lat=NULL, lon=NULL, measurement_name="", directoryname="", basename="", plotstyle=NULL) {
## functionality: process the plotting of an given resultlist-part by qat_plot_boot_distribution
## author: André Düsterhus
## date: 07.06.2010
## version: A0.1
## input: resultlist, optional: the measurement vector, a description name for the measurement vector, a directory for the result, a basic name for the results, and a plotstyle element
## output: plots
	filename <- paste(basename,"_",resultlist_part$element,"_",'bootdist',sep="")
	qat_plot_boot_distribution_1d(resultlist_part$result$stat, filename, resultlist_part$result$bootruns, measurement_name=measurement_name, directoryname=directoryname, plotstyle=plotstyle)
}
