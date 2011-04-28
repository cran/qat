qat_add_comment <-
function(workflowlist, listelem, comment_text) {
# functionality: add or replace a comment to a test
# author: André Düsterhus
# date: 16.03.2010
# version: A0.1
# input: to do-list of analysing steps
# output: edited to do-list of analysing steps
 library("XML")
	bool_added <- FALSE
	# in the workflowlist a comment should be stored under $additional_information$result$comment_on_result
	addinfo_elem<-which(names(workflowlist[[listelem]]) == "additional_information")
	if (length(addinfo_elem)>1) {
		# for-cycle over all addinfo-elements is usable, because more than 1 addinfo-element
		for (ii in 1:length(addinfo_elem)) {
			result_elem <- which(names(workflowlist[[listelem]][[addinfo_elem[ii]]])=="result")	
			if(length(result_elem)>1) {
				# for-cycle over all result-elements is usable, because more than 1 result-element
				for (jj in 1: length(result_elem)) {
					if(names(workflowlist[[listelem]][[addinfo_elem[ii]]][[result_elem[jj]]])=="comment_on_result") {
						# comment exist so it have to be replaced
						workflowlist[[listelem]][[addinfo_elem[ii]]][[result_elem[jj]]][1] <- comment_text
						bool_added <- TRUE
					}	
				}
				if (!bool_added) {
					# no comment exist, but multiple results, so add it
					add_comment <- list(comment_text)
					names(add_comment) <- "comment_on_result"
					add_result<- list(add_comment)
					names(add_result) <- "result"
					workflowlist[[listelem]][[addinfo_elem[ii]]] <- c(workflowlist[[listelem]][[addinfo_elem[ii]]],add_result)
					bool_added <- TRUE
				}
			}
			if(length(result_elem)==1) {
				# use result-element directly
				print(names(workflowlist[[listelem]][[addinfo_elem[ii]]][[result_elem]]))
				if(names(workflowlist[[listelem]][[addinfo_elem[ii]]][[result_elem]])=="comment_on_result") {
					# comment exist so it have to be replaced
					workflowlist[[listelem]][[addinfo_elem[ii]]][[result_elem]][1] <- comment_text
					bool_added <- TRUE
				} else {
					# no comment exist, but a result, so add it
					add_comment <- list(comment_text)
					names(add_comment) <- "comment_on_result"
					add_result<- list(add_comment)
					names(add_result) <- "result"
					workflowlist[[listelem]][[addinfo_elem[ii]]] <- c(workflowlist[[listelem]][[addinfo_elem[ii]]],add_result)
					bool_added <- TRUE
				}
			}
		}
		if (!bool_added) {
			#create new addinfo element
			add_comment <- list(comment_text)
			names(add_comment) <- "comment_on_result"
			add_result<- list(add_comment)
			names(add_result) <- "result"
			add_addinfo <- list(add_result)
			names(add_addinfo) <- "additional_information"
			workflowlist[[listelem]] <- c(workflowlist[[listelem]],add_addinfo)
			bool_added <- TRUE

		}
	} else {
		# 0 or 1 addinfo-elements
		if (length(addinfo_elem)==1) {
			#take addinfo_elem directly
			result_elem <- which(names(workflowlist[[listelem]][[addinfo_elem]])=="result")	
			if(length(result_elem)>1) {
				# for-cycle over all result-elements is usable, because more than 1 result-element
				for (jj in 1: length(result_elem)) {
					if(names(workflowlist[[listelem]][[addinfo_elem]][[result_elem[jj]]])=="comment_on_result") {
						# comment exist so it have to be replaced
						workflowlist[[listelem]][[addinfo_elem]][[result_elem[jj]]][1] <- comment_text
						bool_added <- TRUE
					}	
				}
				if (!bool_added) {
					# no comment exist, but results, so add it
					add_comment <- list(comment_text)
					names(add_comment) <- "comment_on_result"
					add_result<- list(add_comment)
					names(add_result) <- "result"
					workflowlist[[listelem]][[addinfo_elem]] <- c(workflowlist[[listelem]][[addinfo_elem]],add_result)
					bool_added <- TRUE
			
				}
			}
			if(length(result_elem)==1) {
				# use result-element directly
				if(names(workflowlist[[listelem]][[addinfo_elem]][[result_elem]])=="comment_on_result") {
					# comment exist so it have to be replaced
					workflowlist[[listelem]][[addinfo_elem]][[result_elem]][1] <- comment_text
					bool_added <- TRUE					
				} else {
					# no comment exist, but a result, so add it
					add_comment <- list(comment_text)
					names(add_comment) <- "comment_on_result"
					add_result<- list(add_comment)
					names(add_result) <- "result"
					workflowlist[[listelem]][[addinfo_elem]] <- c(workflowlist[[listelem]][[addinfo_elem]],add_result)
					bool_added <- TRUE
				}
			}
			if (!bool_added) {
				#create new addinfo element
				add_comment <- list(comment_text)
				names(add_comment) <- "comment_on_result"
				add_result<- list(add_comment)
				names(add_result) <- "result"
				add_addinfo <- list(add_result)
				names(add_addinfo) <- "additional_information"
				workflowlist[[listelem]] <- c(workflowlist[[listelem]],add_addinfo)
				bool_added <- TRUE
			}
		} else {
			#create new addinfo element
			add_comment <- list(comment_text)
			names(add_comment) <- "comment_on_result"
			add_result<- list(add_comment)
			names(add_result) <- "result"
			add_addinfo <- list(add_result)
			names(add_addinfo) <- "additional_information"
#			comment_text<-paste(comment_text)
			workflowlist[[listelem]] <- c(workflowlist[[listelem]],add_addinfo)
			bool_added <- TRUE
		}
	}
	return(workflowlist)
}

