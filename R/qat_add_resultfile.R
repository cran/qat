qat_add_resultfile <-
function(workflowlist, listelem, resultfile_text) {
# functionality: add a resultfile to a test
# author: André Düsterhus
# date: 03.07.2010
# version: A0.1
# input: to do-list of analysing steps
# output: edited to do-list of analysing steps
 library("XML")
	bool_added <- FALSE
	# in the workflowlist a comment should be stored under $additional_information$result$comment_on_result
	addinfo_elem<-which(names(workflowlist[[listelem]]) == "additional_information")
	if (length(addinfo_elem)>1) {
		# no resultfile exist, but multiple results, so add it
		add_resultfile <- list(resultfile_text)
		names(add_resultfile) <- "result_file"
		add_result<- list(add_resultfile)
		names(add_result) <- "result"
		workflowlist[[listelem]][[addinfo_elem[1]]] <- c(workflowlist[[listelem]][[addinfo_elem[1]]],add_result)
		bool_added <- TRUE
	} else {
		# 0 or 1 addinfo-elements
		if (length(addinfo_elem)==1) {
			# no comment exist, but results, so add it
			add_resultfile <- list(resultfile_text)
			names(add_resultfile) <- "result_file"
			add_result<- list(add_resultfile)
			names(add_result) <- "result"
			workflowlist[[listelem]][[addinfo_elem]] <- c(workflowlist[[listelem]][[addinfo_elem]],add_result)
			bool_added <- TRUE
		} else {
			#create new addinfo element
			add_resultfile <- list(resultfile_text)
			names(add_resultfile) <- "result_file"
			add_result<- list(add_resultfile)
			names(add_result) <- "result"
			add_addinfo <- list(add_result)
			names(add_addinfo) <- "additional_information"
			workflowlist[[listelem]] <- c(workflowlist[[listelem]],add_addinfo)
			bool_added <- TRUE
		}
	}
	return(workflowlist)
}

