qat_call_plot_histogram_test <-
function(resultlist_part, measurement_vector=NULL, time=NULL, height= NULL, lat=NULL, lon=NULL, measurement_name="", directoryname="", basename="", plotstyle=NULL) {
## functionality: process the plotting of an given resultlist-part by qat_plot_histogram_test_1d
## author: André Düsterhus
## date: 22.03.2013
## version: A0.1
## input: resultlist, optional: the measurement vector, a description name for the measurement vector, a directory for the result, a basic name for the results, and a plotstyle element
## output: plots

	filename <- paste(basename,"_",resultlist_part$element,"_histogramtest_", resultlist_part$result$metric, sep="")
	if ("factorofbar" %in% names(resultlist_part$result)) {
		qat_plot_histogram_test(resultlist_part$result$field, filename, resultlist_part$result$blocksize, resultlist_part$result$numofbars, resultlist_part$result$factorofbar, resultlist_part$result$metric, runs=resultlist_part$result$runs, measurement_name = measurement_name, directoryname=directoryname, plotstyle=plotstyle)
	} else {
		qat_plot_histogram_test(resultlist_part$result$field, filename, resultlist_part$result$blocksize, resultlist_part$result$numofbars, -1, resultlist_part$result$metric, runs=resultlist_part$result$runs, measurement_name = measurement_name, directoryname=directoryname, plotstyle=plotstyle)	
	}
}
