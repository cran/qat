qat_read_parameter <-
function(filename,methodname) {
## functionality: giving back parameters of a method
## author: André Düsterhus
## date: 09.03.2010
## version: A0.2
## input: filename of the description file of the methods, name of the method
## output: a list with a corrected name, name of an analysis function or manipulation function, optional name of a plot function, description of the method and algorithm of the method
	library("XML")
	# this function is not case sesitive
	methodname<-tolower(methodname)
	# read xml file
	xmlfile <- xmlTreeParse(filename)
	numofmethods<-which(names(xmlfile$doc$children$qat_basetools)=="methods")
	# initialise cycle variables
	bool <- TRUE
	ii <- 1
	# cycle until a termination criteria is fullfilled: found a match or checked everything and found no match
	while(bool){
		# find out, which element of the actual method is a name
		name_index<-which(names(xmlfile$doc$children$qat_basetools[[numofmethods]][[ii]]) == "name")
		for (jj in 1:length(name_index)) {
			# check every name tag, if sucessful save indices and set termination criteria
			if (toString(xmlfile$doc$children$qat_basetools[[numofmethods]][[ii]][[name_index[jj]]][[1]])==methodname) {
				bool <- FALSE
				numofname <- jj
				numofmethod <- ii
			}
		}
		if (bool) {
			# when there is no match, check if everything is checked. if yes quit and set indices to 0
			if (ii < length(xmlfile$doc$children$qat_basetools[[numofmethods]])) {
				ii<-ii+1
			} else {
				bool <- FALSE
				numofname<- 0
				numofmethod <- 0
			}
		}
	}
	if ((numofname != 0) && (numofmethod != 0)) {
		# looking for indices of the elements, which should be returned
		# name is included to correcting name problems of xml-file
		name_index <- which(names(xmlfile$doc$children$qat_basetools[[numofmethods]][[numofmethod]]) == "name")
		analysis_index <- which(names(xmlfile$doc$children$qat_basetools[[numofmethods]][[numofmethod]]) == "analysis_function")
		plot_index <- which(names(xmlfile$doc$children$qat_basetools[[numofmethods]][[numofmethod]]) == "plot_function")
		save_index <- which(names(xmlfile$doc$children$qat_basetools[[numofmethods]][[numofmethod]]) == "save_function")
		manipulation_index <- which(names(xmlfile$doc$children$qat_basetools[[numofmethods]][[numofmethod]]) == "manipulation_function")
		desc_index <- which(names(xmlfile$doc$children$qat_basetools[[numofmethods]][[numofmethod]]) == "description")
		algorithm_index <- which(names(xmlfile$doc$children$qat_basetools[[numofmethods]][[numofmethod]]) == "algorithm")
		# preselect content
		if (length(analysis_index) == 1) {
			analysis_content <- toString(xmlfile$doc$children$qat_basetools[[numofmethods]][[numofmethod]][[analysis_index]][[1]])
		} else {
			analysis_content <- NULL
		}
		if (length(plot_index) == 1) {
			plot_content <- toString(xmlfile$doc$children$qat_basetools[[numofmethods]][[numofmethod]][[plot_index]][[1]])
		} else {
			plot_content <- NULL
		}
		if (length(save_index) == 1) {
			save_content <- toString(xmlfile$doc$children$qat_basetools[[numofmethods]][[numofmethod]][[save_index]][[1]])
		} else {
			save_content <- NULL
		}		
		if (length(manipulation_index) == 1) {
			manipulation_content <- toString(xmlfile$doc$children$qat_basetools[[numofmethods]][[numofmethod]][[manipulation_index]][[1]])
		} else {
			manipulation_content <- NULL
		}		
		if (length(desc_index) == 1) {
			desc_content <- toString(xmlfile$doc$children$qat_basetools[[numofmethods]][[numofmethod]][[desc_index]][[1]])
		} else {
			desc_content <- NULL
		}
		if (length(algorithm_index) == 1) {
			algorithm_content <- toString(xmlfile$doc$children$qat_basetools[[numofmethods]][[numofmethod]][[algorithm_index]][[1]])
		} else {
			algorithm_content <- NULL
		}
		# set up list to return elements
		returnlist <- list (name=toString(xmlfile$doc$children$qat_basetools[[numofmethods]][[numofmethod]][[name_index[1]]][[1]]), analysis_function=analysis_content, plot_function=plot_content, save_function=save_content, manipulation_function=manipulation_content, description=desc_content, algorithm=algorithm_content)
	} else {
		returnlist<-NULL
	}
	return(returnlist)
}

