qat_config_read_workflow <-
function(filename) {
## functionality: read xml-config file
## author: André Düsterhus
## date: 21.01.2010
## version: A0.1
## input: ncdf object
## output: to do-list of analysing steps
#	library("XML")
	# initialisation of variables
	workflowlist <- list()
	listcounter <- 0
	#parsing xml-tree
	xmlfile <- xmlTreeParse(filename)
	numofworkflow<-which(names(xmlfile$doc$children$qatfile)=="workflow")
	for (ii in 1:length(xmlfile$doc$children$qatfile[[numofworkflow]])) {
		# search for mathod name
		name_index<-which(names(xmlfile$doc$children$qatfile[[numofworkflow]][[ii]]) == "method_name")
		# look for method and type (procedure or tag)
		workflowlist[[listcounter <- listcounter+1]] <-list(method=toString(xmlfile$doc$children$qatfile[[numofworkflow]][[ii]][[name_index]][[1]]), type=names(xmlfile$doc$children$qatfile[[numofworkflow]])[[ii]])
		# get parameter info
		parameter_index<-which(names(xmlfile$doc$children$qatfile[[numofworkflow]][[ii]]) == "parameter")
		if (length(parameter_index) != 0) {
			for (jj in 1:length(parameter_index)) {
				parametervalue_index <- which(names(xmlfile$doc$children$qatfile[[numofworkflow]][[ii]][[parameter_index[jj]]])=="parameter_value")
				templist <- list(toString(xmlfile$doc$children$qatfile[[numofworkflow]][[ii]][[parameter_index[jj]]][[parametervalue_index]][[1]]))
				parametername_index <- which(names(xmlfile$doc$children$qatfile[[numofworkflow]][[ii]][[parameter_index[jj]]])=="parameter_name")
				names(templist)<-toString(xmlfile$doc$children$qatfile[[numofworkflow]][[ii]][[parameter_index[jj]]][[parametername_index]][[1]]) 
				workflowlist[[listcounter]] <- c(workflowlist[[listcounter]],templist)
			}
		}
		addinfo_list<-list()
		addinfo_counter <- 0
		#still to do: result, algorithm, description
		result_index<-which(names(xmlfile$doc$children$qatfile[[numofworkflow]][[ii]]) == "result")
		if (length(result_index)!=0) {
			resultlist <- list()
			resultlist_counter <- 0
			result_comment_index <- which(names(xmlfile$doc$children$qatfile[[numofworkflow]][[ii]][[result_index]]) == "comment_on_result")
			if (length(result_comment_index) != 0) { 
				result_comment_list <- list(toString(xmlfile$doc$children$qatfile[[numofworkflow]][[ii]][[result_index]][[result_comment_index]][[1]]))
				names(result_comment_list) <- "comment_on_result"
				resultlist[[resultlist_counter<-resultlist_counter+1]] <- result_comment_list
			}
			result_file_index <- which(names(xmlfile$doc$children$qatfile[[numofworkflow]][[ii]][[result_index]]) == "result_file")
			if (length(result_file_index) != 0) {
				for (jj in 1:length(result_file_index)) {
					result_file_list <- list(toString(xmlfile$doc$children$qatfile[[numofworkflow]][[ii]][[result_index]][[result_file_index[jj]]][[1]]))	
					names(result_file_list)<-"result_file"
					resultlist[[resultlist_counter<-resultlist_counter+1]] <- result_file_list					
				}
			}
			names(resultlist) <- array("result",resultlist_counter)
			addinfo_list[[addinfo_counter<-addinfo_counter+1]] <- resultlist
#			addinfo_list <- c(addinfo_list,resultlist)
		}
		description_index<-which(names(xmlfile$doc$children$qatfile[[numofworkflow]][[ii]]) == "description")
		if (length(description_index)!=0) {
			description_list<-list(toString(xmlfile$doc$children$qatfile[[numofworkflow]][[ii]][[description_index]][[1]]))
			names(description_list)<- "description"
			addinfo_list[[addinfo_counter<-addinfo_counter+1]] <- description_list
#			addinfo_list <- c(addinfo_list,description_list)
		}
		algorithm_index<-which(names(xmlfile$doc$children$qatfile[[numofworkflow]][[ii]]) == "algorithm")
		if (length(algorithm_index)!=0) {
			algorithm_list<-list(toString(xmlfile$doc$children$qatfile[[numofworkflow]][[ii]][[algorithm_index]][[1]]))
			names(algorithm_list)<- "algorithm"
			addinfo_list[[addinfo_counter<-addinfo_counter+1]] <- algorithm_list
		}
		names(addinfo_list)<-array("additional_information",addinfo_counter)
		workflowlist[[listcounter]]<-c(workflowlist[[listcounter]],addinfo_list)
	}
	return(workflowlist)
}
