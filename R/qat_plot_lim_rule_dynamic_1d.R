qat_plot_lim_rule_dynamic_1d <-
function(flagvector, filename, measurement_vector=NULL, min_vector=NULL, max_vector=NULL, min_vector_name=NULL, max_vector_name=NULL, measurement_name="", directoryname="", plotstyle=NULL) {
## functionality: plot lim-dynamic-rule
## author: André Düsterhus
## date: 24.02.2010
## version: A0.1
## input: flagvector from qat_analyse_lim_rule_dynamic, measurement_vector, min_value, max_value, directoryname, filename, plotstylelist
## output: plot
#	library("gplots")
	if (is.null(plotstyle)) {
		# if no plotstyle available, use standard plotstyle
		plotstyle<-qat_style_plot()
	}
	# set up savepath of the plot
	path <- paste(directoryname,filename,".png", sep="")
	if (length(measurement_vector) == 0 || (sum(is.nan(measurement_vector))==length(measurement_vector))) {
		png(filename=path,width=800,height=600, pointsize=12, bg=plotstyle$basecolor)
			par(font.lab=2,oma=c(0,0,2,0))
			plot(flagvector, col=plotstyle$plotcolorminor, xlab="", ylab="", type="p", col.lab=plotstyle$fontcolor, col.main=plotstyle$fontcolor, col.sub=plotstyle$fontcolor, fg=plotstyle$frontcolor, col.axis=plotstyle$fontcolor, font.axis=2)
			title(main=list("LIM-Rule dynamic", col=plotstyle$fontcolor), outer=TRUE)
		dev.off()
	} else {
		png(filename=path,width=800,height=600, pointsize=12, bg=plotstyle$basecolor)
			par(font.lab=2,oma=c(0,0,2,0))
			plot(which(flagvector != 0),measurement_vector[which((flagvector != 0))], col=plotstyle$plotcolormain, xlab="", ylab="", type="n", col.lab=plotstyle$fontcolor, col.main=plotstyle$fontcolor, col.sub=plotstyle$fontcolor,fg=plotstyle$frontcolor, col.axis=plotstyle$fontcolor, pch=plotstyle$plotpointmain, font.axis=2, xlim=c(1, length(flagvector)), ylim=c(min(measurement_vector,na.rm =TRUE),max(measurement_vector,na.rm =TRUE)))
			if((length(min_vector) != 0) && (length(max_vector) != 0)) {
				rect(which(!is.na(min_vector)==!is.na(max_vector))-0.5,min_vector[which(!is.na(min_vector)==!is.na(max_vector))],which( !is.na(min_vector)==!is.na(max_vector))+0.5,max_vector[which(!is.na(min_vector)==!is.na(max_vector))], lwd=0, border = NA,col =plotstyle$plotcolorbackground)
				points(which(xor(!is.na(min_vector),!is.na(max_vector))), min_vector[which(xor(!is.na(min_vector),!is.na(max_vector)))], col=plotstyle$frontcolor, pch="-")
				points(which(xor(!is.na(min_vector),!is.na(max_vector))), max_vector[which(xor(!is.na(min_vector),!is.na(max_vector)))], col=plotstyle$frontcolor, pch="-")
			}
			if(length(min_vector) != 0) { #&& length(max_vector) == 0
				points(which(!is.na(min_vector)),min_vector[!is.na(min_vector)], col=plotstyle$frontcolor,pch="-")
			}
			if(length(max_vector) != 0) { #length(min_vector) == 0 && 
				points(which(!is.na(max_vector)),max_vector[!is.na(max_vector)], col=plotstyle$frontcolor,pch="-")
			}
			points(which(flagvector != 0),measurement_vector[which((flagvector != 0))], col=plotstyle$plotcolormain, xlab="", ylab="", type="p", col.lab=plotstyle$fontcolor, col.main=plotstyle$fontcolor, col.sub=plotstyle$fontcolor,fg=plotstyle$frontcolor, col.axis=plotstyle$fontcolor, pch=plotstyle$plotpointmain, font.axis=2, xlim=c(1, length(flagvector)), ylim=c(min(measurement_vector,na.rm =TRUE),max(measurement_vector,na.rm =TRUE)))

			points(which(flagvector == 0), measurement_vector[which(flagvector == 0)], col=plotstyle$plotcolorminor, col.lab=plotstyle$fontcolor,  pch=plotstyle$plotpointminor)

			# set title of the plot
			title(main=list("LIM-Rule dynamic", col=plotstyle$fontcolor), outer=TRUE)
			# information text at the top of the plot
			if (length(min_vector_name) != 0) {
				bordertext <- paste("Minimum vector: ", min_vector_name, " ")
				bordertext2<- paste("Minimum errors: ",length(which(flagvector==-1)), " ")
			} else {
				bordertext<- ""
				bordertext2<- ""
			}
			if (length(max_vector_name) != 0) {
				bordertext <- paste(bordertext,"Maximum vector: ", max_vector_name)
				bordertext2<- paste(bordertext2,"Maximum errors: ",length(which(flagvector==1)))
			} else {
				bordertext<- paste(bordertext,"")
				bordertext2<- paste(bordertext2,"")
			}
			if (bordertext != "") {
				mtext(bordertext, side=3, line=2, font=2, col=plotstyle$fontcolor)
				mtext(bordertext2, side=3, line=1, font=2, col=plotstyle$fontcolor)
			}
			if(measurement_name != "") {
				bordertext3<-paste("Data: ",measurement_name, sep="")
				mtext(bordertext3, side=3, line=3, font=2, col=plotstyle$fontcolor)
			}
		dev.off()
	}
}
