qat_call_plot_roc_rule <-
function(resultlist_part, measurement_vector=NULL, time=NULL, height= NULL, lat=NULL, lon=NULL, measurement_name="", directoryname="", basename="", plotstyle=NULL) {
## functionality: process the plotting of an given resultlist-part by qat_plot_roc_rule_static or qat_plot_roc_rule_dynamic
## author: André Düsterhus
## date: 10.03.2010
## version: A0.1
## input: resultlist, optional: the measurement vector, a description name for the measurement vector, a directory for the result, a basic name for the results, and a plotstyle element
## output: plots
	if (resultlist_part$method == 'roc_static') {
		filename<-paste(basename,"_",resultlist_part$element,"_",'roc_static',sep="")
		if (is.null(dim(resultlist_part$result$flagvector))) {	
			qat_plot_roc_rule_static_1d(resultlist_part$result$flagvector, filename, measurement_vector=measurement_vector, max_upward_value=resultlist_part$result$max_upward_value, max_downward_value=resultlist_part$result$max_downward_value, measurement_name=measurement_name, directoryname=directoryname, plotstyle=plotstyle)
		}
		if (length(dim(resultlist_part$result$flagvector))==2) {	
			qat_plot_roc_rule_static_2d(resultlist_part$result$flagvector, filename, measurement_vector=measurement_vector, max_upward_value=resultlist_part$result$max_upward_value, max_downward_value=resultlist_part$result$max_downward_value, measurement_name=measurement_name, directoryname=directoryname, plotstyle=plotstyle)
		}
	}
	if (resultlist_part$method == 'roc_dynamic') {
		filename<-paste(basename,"_",resultlist_part$element,"_",'roc_dynamic',sep="")
		qat_plot_roc_rule_dynamic_1d(resultlist_part$result$flagvector, filename, measurement_vector=measurement_vector, max_upward_vector=resultlist_part$result$max_upward_vector, max_downward_vector=resultlist_part$result$max_downward_vector, upward_vector_name=resultlist_part$result$upward_vector_name, downward_vector_name=resultlist_part$result$downward_vector_name, measurement_name=measurement_name, directoryname=directoryname, plotstyle=plotstyle)
	}
}
