qat_add_all_algorithms <-
function(workflowlist) {
# functionality: add or replace all description in a workflowlist
# author: André Düsterhus
# date: 05.07.2010
# version: A0.1
# input: to do-list of analysing steps
# output: edited to do-list of analysing steps
	if (length(which(names(workflowlist)=="method"))==0) {
		cycleelement <- length(workflowlist)
	} else {
		cycleelement <- 1
	}
	for (ii in 1:cycleelement) {
		if (cycleelement !=1) {
			templist <- workflowlist[[ii]]
		} else {
			templist <- workflowlist[[1]]
		}
		# this call unsures, that the correct file is called, when the package is installed
		filename <- system.file("extdata/qat_basetools.xml", package="qat")
		# testenvironment
#		 filename <- "qat_basetools.xml"
		parameter_info <- qat_read_parameter(filename, trim(templist$method))
		if (length(parameter_info) != 0) {
			# test exists
			if (!is.null(parameter_info$algorithm)) {
				workflowlist <- qat_add_algorithm(workflowlist,ii,parameter_info$algorithm)
			}	
		}
	}
	return(workflowlist)
}
