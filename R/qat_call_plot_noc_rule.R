qat_call_plot_noc_rule <-
function(resultlist_part, measurement_vector=NULL, time=NULL, height= NULL, lat=NULL, lon=NULL, measurement_name="", directoryname="", basename="", plotstyle=NULL) {
## functionality: process the plotting of an given resultlist-part by qat_plot_noc_rule
## author: André Düsterhus
## date: 10.03.2010
## version: A0.1
## input: resultlist, optional: the measurement vector, a description name for the measurement vector, a directory for the result, a basic name for the results, and a plotstyle element
## output: plots
	if (resultlist_part$method == 'noc') {
		filename<-paste(basename,"_",resultlist_part$element,"_",'noc',sep="")
		if (is.null(dim(resultlist_part$result$flagvector))) {	
			qat_plot_noc_rule_1d(resultlist_part$result$flagvector, filename, measurement_vector=measurement_vector, max_return_elements=resultlist_part$result$max_return_elements, measurement_name=measurement_name, directoryname=directoryname, plotstyle=plotstyle)
		}
		if (length(dim(resultlist_part$result$flagvector))==2) {	
			qat_plot_noc_rule_2d(resultlist_part$result$flagvector, filename, measurement_vector=measurement_vector, max_return_elements=resultlist_part$result$max_return_elements, measurement_name=measurement_name, directoryname=directoryname, plotstyle=plotstyle)
		}		
		}
}
