qat_add_algorithm <-
function(workflowlist, listelem, algorithm_text) {
# functionality: add or replace a algorithm to a test
# author: André Düsterhus
# date: 03.07.2010
# version: A0.1
# input: to do-list of analysing steps
# output: edited to do-list of analysing steps
 library("XML")
	bool_added <- FALSE
	# in the workflowlist a description should be stored under $additional_information$description
	addinfo_elem<-which(names(workflowlist[[listelem]]) == "additional_information")
	if (length(addinfo_elem)>1) {
		# for-cycle over all addinfo-elements is usable, because more than 1 addinfo-element
		for (ii in 1:length(addinfo_elem)) {
			algorithm_elem <- which(names(workflowlist[[listelem]][[addinfo_elem[ii]]])=="algorithm")	
			if(length(algorithm_elem)==1) {
				# use existing algorithm-element
				workflowlist[[listelem]][[addinfo_elem[ii]]][[algorithm_elem]][1] <- algorithm_text
				bool_added <- TRUE
			}
		}
		if (!bool_added) {
			#create new addinfo element
			add_algorithm <- list(algorithm_text)
			names(add_algorithm) <- "algorithm"
			add_addinfo <- list(add_algorithm)
			names(add_addinfo) <- "additional_information"
			workflowlist[[listelem]] <- c(workflowlist[[listelem]],add_addinfo)
			bool_added <- TRUE
		}
	} else {
		# 0 or 1 addinfo-elements
		if (length(addinfo_elem)==1) {
			#take addinfo_elem directly
			algorithm_elem <- which(names(workflowlist[[listelem]][[addinfo_elem]])=="algorithm")	
			if(length(algorithm_elem)==1) {
				# use result-element directly
				workflowlist[[listelem]][[addinfo_elem]][[algorithm_elem]][1] <- algorithm_text
				bool_added <- TRUE					
			} else {
				# no algorithm exist, but a addinfo elem
				add_algorithm <- list(algorithm_text)
				names(add_algorithm) <- "algorithm"
				workflowlist[[listelem]][[addinfo_elem]] <- c(workflowlist[[listelem]][[addinfo_elem]],add_algorithm)
				bool_added <- TRUE
			}
		} else {
			#create new addinfo element
			add_algorithm <- list(algorithm_text)
			names(add_algorithm) <- "algorithm"
			add_addinfo <- list(add_algorithm)
			names(add_addinfo) <- "additional_information"
			workflowlist[[listelem]] <- c(workflowlist[[listelem]],add_addinfo)
			bool_added <- TRUE
		}
	}
	return(workflowlist)
}

