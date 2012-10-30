qat_call_plot_distribution <-
function(resultlist_part, measurement_vector=NULL, time=NULL, height= NULL, lat=NULL, lon=NULL, measurement_name="", directoryname="", basename="", plotstyle=NULL) {
## functionality: process the plotting of an given resultlist-part by qat_analyse_distribution_1d
## author: André Düsterhus
## date: 03.05.2010
## version: A0.1
## input: resultlist, optional: the measurement vector, a description name for the measurement vector, a directory for the result, a basic name for the results, and a plotstyle element
## output: plots
		filename<-paste(basename,"_",resultlist_part$element,"_",'dist',sep="")
		qat_plot_distribution_1d(resultlist_part$result$hist, filename, resultlist_part$result$stat, resultlist_part$result$numofbars, measurement_name=measurement_name, directoryname=directoryname, plotstyle=plotstyle)
	
}
