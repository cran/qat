qat_add_description <-
function(workflowlist, listelem, description_text) {
# functionality: add or replace a description to a test
# author: André Düsterhus
# date: 03.07.2010
# version: A0.1
# input: to do-list of analysing steps
# output: edited to do-list of analysing steps
# library("XML")
	bool_added <- FALSE
	# in the workflowlist a description should be stored under $additional_information$description
	addinfo_elem<-which(names(workflowlist[[listelem]]) == "additional_information")
	if (length(addinfo_elem)>1) {
		# for-cycle over all addinfo-elements is usable, because more than 1 addinfo-element
		for (ii in 1:length(addinfo_elem)) {
			description_elem <- which(names(workflowlist[[listelem]][[addinfo_elem[ii]]])=="description")	
			if(length(description_elem)==1) {
				# use existing description-element
				workflowlist[[listelem]][[addinfo_elem[ii]]][[description_elem]][1] <- description_text
				bool_added <- TRUE
			}
		}
		if (!bool_added) {
			#create new addinfo element
			add_description <- list(description_text)
			names(add_description) <- "description"
			add_addinfo <- list(add_description)
			names(add_addinfo) <- "additional_information"
			workflowlist[[listelem]] <- c(workflowlist[[listelem]],add_addinfo)
			bool_added <- TRUE
		}
	} else {
		# 0 or 1 addinfo-elements
		if (length(addinfo_elem)==1) {
			#take addinfo_elem directly
			description_elem <- which(names(workflowlist[[listelem]][[addinfo_elem]])=="description")	
			if(length(description_elem)==1) {
				# use result-element directly
				workflowlist[[listelem]][[addinfo_elem]][[description_elem]][1] <- description_text
				bool_added <- TRUE					
			} else {
				# no description exist, but a addinfo elem
				add_description <- list(description_text)
				names(add_description) <- "description"
				workflowlist[[listelem]][[addinfo_elem]] <- c(workflowlist[[listelem]][[addinfo_elem]],add_description)
				bool_added <- TRUE
			}
		} else {
			#create new addinfo element
			add_description <- list(description_text)
			names(add_description) <- "description"
			add_addinfo <- list(add_description)
			names(add_addinfo) <- "additional_information"
			workflowlist[[listelem]] <- c(workflowlist[[listelem]],add_addinfo)
			bool_added <- TRUE
		}
	}
	return(workflowlist)
}
