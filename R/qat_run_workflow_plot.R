qat_run_workflow_plot <-
function(resultlist, measurement_name="", directoryname="", basename="", plotstyle=NULL) {
## functionality: process the plotting of an given resultlist by qat_run_workflow_check
## author: André Düsterhus
## date: 10.03.2010
## version: A0.1
## input: resultlist, optional: a description name for the emasurement vector, a directory for the result, a basic name for the results, and a plotstyle element
## output: plots
	if (length(which(names(resultlist)=="element"))==0) {
		cycleelement <- length(resultlist)
	} else {
		cycleelement <- 2
	}
	print("plot")
	for (ii in 2:cycleelement) {
		print(ii)
		if (cycleelement != 2) {
			templist <- resultlist[[ii]]
		} else {
			templist <- resultlist[[2]]
		}
		# this call ensures, that the correct file is called, when the package is installed
		filename <- system.file("extdata/qat_basetools.xml", package="qat")
		parameter_info <- qat_read_parameter(filename, trim(templist$method))
		if (length(parameter_info) != 0) {
			if (!is.null(parameter_info$plot_function)) {
				do.call(parameter_info$plot_function,list(templist, measurement_vector=resultlist[[1]]$measurement_vector, time=resultlist[[1]]$time, height=resultlist[[1]]$height,lat=resultlist[[1]]$lat,lon=resultlist[[1]]$lon, measurement_name=measurement_name, directoryname=directoryname, basename=basename, plotstyle=plotstyle))
			}
		}
	}
}

