qat_style_plot <-
function(filename="") {
## functionality: compile or read a plotstylelist
## author: André Düsterhus
## date: 29.07.2010
## version: A0.1
## input: optional directoryname and filename of a plotlist-xml
## output: list with: information on
	if (filename == "") {
		basecolor<-"white"
		frontcolor<-"black"
		plotcolormain<-"red"
		plotcolorminor<-"black"
		plotcolorbackground<-"lightgrey"
		fontcolor<-"black"
		plotpointminor <- 20
		plotpointmain <- 3
		plotlist <- list()
		plotlist<- c(list(basecolor),list(frontcolor),list(plotcolormain), list(plotcolorminor),list(plotcolorbackground), list(fontcolor), list(plotpointmain), list(plotpointminor))
		names(plotlist)<-c("basecolor","frontcolor","plotcolormain","plotcolorminor", "plotcolorbackground", "fontcolor", "plotpointmain", "plotpointminor")
	} else {
#		filename_bonded<- paste(directoryname, filename,sep="")
		plotlist <- list()
		xmlfile <- xmlTreeParse(filename)
		parameter_index<-which(names(xmlfile$doc$children$qat_plotstyle) == "parameter")
		for (jj in 1:length(parameter_index)) {
			parametervalue_index <- which(names(xmlfile$doc$children$qat_plotstyle[[parameter_index[jj]]])=="parameter_value")
			templist <- list(toString(xmlfile$doc$children$qat_plotstyle[[parameter_index[jj]]][[parametervalue_index]][[1]]))
			parametername_index <- which(names(xmlfile$doc$children$qat_plotstyle[[parameter_index[jj]]])=="parameter_name")
			names(templist)<-toString(xmlfile$doc$children$qat_plotstyle[[parameter_index[jj]]][[parametername_index]][[1]]) 
			plotlist <- c(plotlist,templist)
		}
	}
	return(plotlist)
}

