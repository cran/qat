qat_call_plot_lim_rule <-
function(resultlist_part, measurement_vector=NULL, time=NULL, height= NULL, lat=NULL, lon=NULL, measurement_name="", directoryname="", basename="", plotstyle=NULL) {
## functionality: process the plotting of an given resultlist-part by qat_plot_lim_rule_static, qat_plot_lim_rule_sigma or qat_plot_lim_rule_dynamic
## author: André Düsterhus
## date: 10.03.2010
## version: A0.1
## input: resultlist, optional: the measurement vector, a description name for the measurement vector, a directory for the result, a basic name for the results, and a plotstyle element
## output: plots
	if (resultlist_part$method == 'lim_static') {
		filename<-paste(basename,"_",resultlist_part$element,"_",'lim_static',sep="")
		if (is.null(dim(resultlist_part$result$flagvector))) {
			qat_plot_lim_rule_static_1d(resultlist_part$result$flagvector, filename, measurement_vector=measurement_vector, min_value=resultlist_part$result$min_value, max_value=resultlist_part$result$max_value, measurement_name=measurement_name, directoryname=directoryname, plotstyle=plotstyle)
		}
		if (length(dim(resultlist_part$result$flagvector))==2) {
			qat_plot_lim_rule_static_2d(resultlist_part$result$flagvector, filename, measurement_vector=measurement_vector, min_value=resultlist_part$result$min_value, max_value=resultlist_part$result$max_value, measurement_name=measurement_name, directoryname=directoryname, plotstyle=plotstyle)
		}
	}
	if (resultlist_part$method == 'lim_sigma') {
		filename<-paste(basename,"_",resultlist_part$element,"_",'lim_sigma',sep="")
		if (is.null(dim(resultlist_part$result$flagvector))) {	
			qat_plot_lim_rule_sigma_1d(resultlist_part$result$flagvector, filename, measurement_vector=measurement_vector, sigma_factor=resultlist_part$result$sigma_factor, meanofvector=resultlist_part$result$meanofvector, sdofvector=resultlist_part$result$sdofvector, measurement_name=measurement_name, directoryname=directoryname, plotstyle=plotstyle)
		}
		if (length(dim(resultlist_part$result$flagvector))==2) {
			qat_plot_lim_rule_sigma_2d(resultlist_part$result$flagvector, filename, measurement_vector=measurement_vector, sigma_factor=resultlist_part$result$sigma_factor, meanofvector=resultlist_part$result$meanofvector, sdofvector=resultlist_part$result$sdofvector, measurement_name=measurement_name, directoryname=directoryname, plotstyle=plotstyle)			
		}		
	}
	if (resultlist_part$method == 'lim_dynamic') {
		filename<-paste(basename,"_",resultlist_part$element,"_",'lim_dynamic',sep="")
		if (is.null(dim(resultlist_part$result$flagvector))) {	
			qat_plot_lim_rule_dynamic_1d(resultlist_part$result$flagvector, filename, measurement_vector=measurement_vector, min_vector=resultlist_part$result$min_vector, max_vector=resultlist_part$result$max_vector, min_vector_name=resultlist_part$result$min_vector_name, max_vector_name=resultlist_part$result$max_vector_name, measurement_name=measurement_name, directoryname=directoryname, plotstyle=plotstyle)
		}
		if (length(dim(resultlist_part$result$flagvector))==2) {
			qat_plot_lim_rule_dynamic_1d(resultlist_part$result$flagvector, filename, measurement_vector=measurement_vector, min_vector=resultlist_part$result$min_vector, max_vector=resultlist_part$result$max_vector, min_vector_name=resultlist_part$result$min_vector_name, max_vector_name=resultlist_part$result$max_vector_name, measurement_name=measurement_name, directoryname=directoryname, plotstyle=plotstyle)
		}
	}	
}
