qat_plot_noc_rule_1d <-
function(flagvector, filename, measurement_vector=NULL, max_return_elements=0, measurement_name="", directoryname="", plotstyle=NULL) {
## functionality: plot noc-rule
## author: André Düsterhus
## date: 24.02.2010
## version: A0.1
## input: flagvector from qat_analyse_noc_rule, measurement_vector, directoryname, filename, plotstylelist
## output: plot
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
			title(main=list("NOC-Rule", col=plotstyle$fontcolor), outer=TRUE)
		dev.off()
	} else {
		png(filename=path,width=800,height=600, pointsize=12, bg=plotstyle$basecolor)
			par(font.lab=2,oma=c(0,0,2,0))
			plot(measurement_vector, col=plotstyle$plotcolormain, xlab="", ylab="", type="n", col.lab=plotstyle$fontcolor, col.main=plotstyle$fontcolor, col.sub=plotstyle$fontcolor,fg=plotstyle$frontcolor, col.axis=plotstyle$fontcolor, pch=plotstyle$plotpointmain, font.axis=2)
			points(which(flagvector != 0), measurement_vector[which(flagvector != 0)], col=plotstyle$plotcolormain, col.lab=plotstyle$fontcolor, pch=plotstyle$plotpointmain)
			points(which(flagvector == 0), measurement_vector[which(flagvector == 0)], col=plotstyle$plotcolorminor, col.lab=plotstyle$fontcolor, pch=plotstyle$plotpointminor)
			title(main=list("NOC-Rule", col=plotstyle$fontcolor), outer=TRUE)
			if (max_return_elements > 0) {
				bordertext <- paste("max. number of returning elements: ", max_return_elements)
				mtext(bordertext, side=3, line=2, font=2, col=plotstyle$fontcolor)
			}	
			bordertext2<- paste("Repetition errors: ",length(which(flagvector==1)), " ")
			mtext(bordertext2, side=3, line=1, font=2, col=plotstyle$fontcolor)
			if(measurement_name != "") {
				bordertext3<-paste("Data: ",measurement_name, sep="")
				mtext(bordertext3, side=3, line=3, font=2, col=plotstyle$fontcolor)
			}		
		dev.off()
	}
}
