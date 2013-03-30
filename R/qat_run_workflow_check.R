qat_run_workflow_check <-
function(measurement_vector, workflowlist, time=NULL, height= NULL, lat=NULL, lon=NULL, vec1=NULL,vec2=NULL,vec3=NULL,vec4=NULL) {
## functionality: process a workflow on given vectors
## author: André Düsterhus
## date: 09.03.2010
## version: A0.1
## input: measurement vector, workflowlist, optional: time vector, latitude vector, longitude vector, additional vectors
## output: a list with results
	library("gdata")
	resultlist <- list()
	resultlistcounter <- 1
	if (length(which(names(workflowlist)=="method"))==0) {
		cycleelement <- length(workflowlist)
	} else {
		cycleelement <- 1
	}
	print("analyse")
	for (ii in 1:cycleelement) {
		print(ii)
		if (cycleelement !=1) {
			templist <- workflowlist[[ii]]
		} else {
			templist <- workflowlist[[1]]
		}
		# this call unsures, that the correct file is called, when the package is installed
		filename <- system.file("extdata/qat_basetools.xml", package="qat")
#		filename <-"../include/inst/extdata/qat_basetools.xml"
		parameter_info <- qat_read_parameter(filename, trim(templist$method))
		if (length(parameter_info) != 0) {
			# the tests exists
			if (!is.null(parameter_info$analysis_function)) {
				# it is a analysis test
				resultlist <- do.call(parameter_info$analysis_function,list(measurement_vector, templist, element=ii,time=time, height=height ,lat=lat, lon=lon, vec1=vec1,vec2=vec2,vec3=vec3,vec4=vec4, resultlist=resultlist, resultlistcounter=resultlistcounter))
				if (length(which(names(workflowlist)=="element"))==0) {
					resultlistcounter<-length(resultlist)
				} else {
					resultlistcounter<-1		
				}
			} else {
				# no analysis test so this have to be a manipulation function
				resultlist <- do.call(parameter_info$manipulation_function,list(measurement_vector, templist, element=ii,time=time, height= height, lat=lat, lon=lon, vec1=vec1,vec2=vec2,vec3=vec3,vec4=vec4, resultlist=resultlist, resultlistcounter=resultlistcounter))	
				if (length(which(names(workflowlist)=="element"))==0) {
					resultlistcounter<-length(resultlist)
				} else {
					resultlistcounter<-1		
				}
				measurement_vector <- resultlist[[resultlistcounter]]$result$measurement_vector
			}
		}		
	}
	resultlist[[1]]<- list(measurement_vector=measurement_vector, time=time,height=height,lat=lat,lon=lon)
	return(resultlist)
}
