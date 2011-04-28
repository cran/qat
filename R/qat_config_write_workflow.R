qat_config_write_workflow <-
function(workflowlist, name="", description="", author="", date="", sample_time_start="", sample_time_stop="", sample_place="", config_filename="", output_filename="") {
## functionality: write xml-result file
## author: André Düsterhus
## date: 08.12.2009
## version: A0.1
## input: ncdf object
## output: to do-list of analysing steps
	library("XML")
	# initialisation of variables
	top <- newXMLNode("qatfile")
	header<- newXMLNode("header", parent = top)
	if (name!="") {
		newXMLNode("name", name ,parent = header)
	}
	newXMLNode("type", "result" , parent = header)
	if (description!="") {
		newXMLNode("description", description, parent = header)
	}
	if (author!="") {
		newXMLNode("author", author, parent = header)
	}
	if (date!="") {
		newXMLNode("date", date, parent = header)
	}
	if (sample_time_start!="") {
		newXMLNode("sample_time_start", sample_time_start, parent = header)
	}
	if (sample_time_stop!="") {
		newXMLNode("sample_time_stop", sample_time_stop, parent = header)
	}
	if (sample_place!="") {
		newXMLNode("sample_place", sample_time_stop, parent = header)
	}
	if (config_filename!="") {
		xmlfile <- xmlTreeParse(config_filename)
		numofheader <- which(names(xmlfile$doc$children$qatfile)=="header")
		config_header<- newXMLNode("config_header", parent = header)
		for (ii in 1:length(xmlfile$doc$children$qatfile[[numofheader]])){
			textarray<-as.character(xmlfile$doc$children$qatfile[[numofheader]][[ii]][[1]])
			newXMLNode(names(xmlfile$doc$children$qatfile[[numofheader]])[ii],textarray[length(textarray)], parent=config_header)
		}
	}
	workflow <- newXMLNode("workflow", parent = top)
	for (ii in 1:(length(workflowlist))) {
		act_method <- newXMLNode(workflowlist[[ii]]$type, parent = workflow)
		is_method_elem <- which(names(workflowlist[[1]])=="method")
		is_type_elem <- which(names(workflowlist[[1]])=="type")
		newXMLNode("method_name",workflowlist[[ii]][is_method_elem],parent = act_method)
		for (jj in 1:length(workflowlist[[ii]])) {
#			print(names(workflowlist[[ii]])[[jj]])
			is_addinfo_elem <- which(names(workflowlist[[ii]])=="additional_information")
#			print(is_addinfo_elem)
			if ((jj != is_method_elem) && (jj != is_type_elem) && !(jj %in% is_addinfo_elem)) {
#				print(names(workflowlist[[ii]])[[jj]])
				act_parameter<-newXMLNode("parameter",parent = act_method)
#				textarray<-as.character(workflowlist[[ii]][[jj]])
				newXMLNode("parameter_name",names(workflowlist[[ii]])[jj],parent = act_parameter)
				newXMLNode("parameter_value",workflowlist[[ii]][jj],parent = act_parameter)
			}
			if (jj %in% is_addinfo_elem) {
				if (length(which(names(workflowlist[[ii]][[jj]]) == "result"))>0) {
					act_result<-newXMLNode("result",parent = act_method)
					for (kk in 1:length(workflowlist[[ii]][[jj]])) {
						newXMLNode(names(workflowlist[[ii]][[jj]][[kk]]),toString(workflowlist[[ii]][[jj]][[kk]][1]),parent = act_result)		
					}
				}
				if (length(which(names(workflowlist[[ii]][[jj]]) == "description"))>0) {
					newXMLNode("description",toString(workflowlist[[ii]][[jj]][[which(names(workflowlist[[ii]][[jj]]) == "description")]]), parent = act_method)
				}
				if (length(which(names(workflowlist[[ii]][[jj]]) == "algorithm"))>0) {
					newXMLNode("algorithm",toString(workflowlist[[ii]][[jj]][[which(names(workflowlist[[ii]][[jj]]) == "algorithm")]]), parent = act_method)
				}
			}
		}
	}
	newXMLDoc(top)
	if (output_filename != "") {
		write(saveXML(top),file=output_filename)
	}
	return(top)
}

