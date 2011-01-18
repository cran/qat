qat_run_workflow_save <-
function(resultlist, baseunit="", time=NULL, height= NULL, lat=NULL, lon=NULL, vec1=NULL,vec2=NULL,vec3=NULL,vec4=NULL) {
## functionality: process a workflow on given vectors
## author: André Düsterhus
## date: 09.03.2010
## version: A0.1
## input: measurement vector, workflowlist, optional: time vector, latitude vector, longitude vector, additional vectors
## output: a list with results
	library("gdata")
	savelist <- list()
	savelistcounter <- 1
	cycleelement <- length(resultlist)
	print("save (netCDF)")
	for (ii in 2:cycleelement) {
		print(ii)
		if (cycleelement !=1) {
			templist <- resultlist[[ii]]
		} else {
			templist <- resultlist[[1]]
		}
		# this call ensures, that the correct file is called, when the package is installed
		filename <- system.file("extdata/qat_basetools.xml", package="qat")
#		filename <- "extdata/qat_basetools.xml"
		parameter_info <- qat_read_parameter(filename, trim(templist$method))
		if (length(parameter_info) != 0) {
			# the tests exists
			if (!is.null(parameter_info$save_function)) {
				# it is a analysis test
				savelist <- do.call(parameter_info$save_function,list(templist, baseunit, element=resultlist[[ii]]$element, time=time, height=height ,lat=lat, lon=lon, vec1=vec1, vec2=vec2, vec3=vec3, vec4=vec4, savelist=savelist, savelistcounter=savelistcounter))
				if (length(which(names(savelist)=="element"))==0) {
					savelistcounter<-length(savelist)
				} else {
					savelistcounter<-1		
				}
			} 
		}
	}
#	savelist[[1]]<- list(transformationonvariable=transformationonvariable)
	return(savelist)
}
