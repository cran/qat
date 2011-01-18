qat_plot_lim_rule_static_1d <-
function(flagvector, filename, measurement_vector=NULL, min_value=NULL, max_value=NULL, measurement_name="", directoryname="", plotstyle=NULL) {
## functionality: plot lim-static-rule
## author: André Düsterhus
## date: 24.02.2010
## version: A0.1
## input: flagvector from qat_analyse_lim_rule_static, measurement_vector, min_value, max_value, directoryname, filename, plotstylelist
## output: plot
	if (is.null(plotstyle)) {
		# if no plotstyle available, use standard plotstyle
		plotstyle<-qat_style_plot()
	}
	# set up savepath of the plot
	if ((length(measurement_vector) == 0) || (sum(is.nan(measurement_vector))==length(measurement_vector))) {
		path <- paste(directoryname,filename,".png", sep="")
		png(filename=path,width=800,height=600, pointsize=12, bg=plotstyle$basecolor)
			par(font.lab=2,oma=c(0,0,2,0))
			plot(flagvector, col=plotstyle$plotcolorminor, xlab="", ylab="", type="p", col.lab=plotstyle$fontcolor, col.main=plotstyle$fontcolor, col.sub=plotstyle$fontcolor, fg=plotstyle$frontcolor, col.axis=plotstyle$fontcolor, font.axis=2)
			title(main=list("LIM-Rule static", col=plotstyle$fontcolor), outer=TRUE)
		dev.off()
	} else {
		path <- paste(directoryname,filename,".png", sep="")
		png(filename=path,width=800,height=600, pointsize=12, bg=plotstyle$basecolor)
			par(font.lab=2,oma=c(0,0,2,0))
			plot(measurement_vector, col=plotstyle$plotcolormain, xlab="", ylab="", type="n", col.lab=plotstyle$fontcolor, col.main=plotstyle$fontcolor, col.sub=plotstyle$fontcolor,fg=plotstyle$frontcolor, col.axis=plotstyle$fontcolor, pch=plotstyle$plotpointmain, font.axis=2,xlim=c(1, length(flagvector)), ylim=c(min(measurement_vector,na.rm =TRUE),max(measurement_vector,na.rm =TRUE)))
			if ((length(min_value) != 0)&&(length(max_value) != 0)) {
				rect(0,min_value,length(measurement_vector),max_value, lwd=0, border = NA,col =plotstyle$plotcolorbackground)	
			}
			if(length(min_value) != 0) {
				abline(h=min_value, col=plotstyle$frontcolor)
				bordertext<- paste("Minimum value: ", min_value, " ")
				bordertext2<- paste("Minimum errors: ", length(which(flagvector==-1)), " ")
			} else {
				bordertext <- ""
				bordertext2 <- ""
			}
			if(length(max_value) != 0) {
				bordertext <- paste(bordertext,"Maximum value: ", max_value)
				bordertext2<- paste(bordertext2,"Maximum errors: ", length(which(flagvector==1)))
				abline(h=max_value, col=plotstyle$frontcolor)
			} else {
				bordertext<- paste(bordertext,"")
				bordertext2<- paste(bordertext2,"")
			}
			points(which(flagvector != 0), measurement_vector[which(flagvector != 0)], col=plotstyle$plotcolormain, xlab="", ylab="", type="p", col.lab=plotstyle$fontcolor, col.main=plotstyle$fontcolor, col.sub=plotstyle$fontcolor,fg=plotstyle$frontcolor, col.axis=plotstyle$fontcolor, pch=plotstyle$plotpointmain)
			points(which(flagvector == 0), measurement_vector[which(flagvector == 0)], col=plotstyle$plotcolorminor, xlab="", ylab="", type="p", col.lab=plotstyle$fontcolor, col.main=plotstyle$fontcolor, col.sub=plotstyle$fontcolor,fg=plotstyle$frontcolor, col.axis=plotstyle$fontcolor, pch=plotstyle$plotpointminor)

			if (bordertext != "") {
				mtext(bordertext, side=3, line=2, font=2, col=plotstyle$fontcolor)
				mtext(bordertext2, side=3, line=1, font=2, col=plotstyle$fontcolor)
			}
			if(measurement_name != "") {
				bordertext3<-paste("Data: ",measurement_name, sep="")
				mtext(bordertext3, side=3, line=3, font=2, col=plotstyle$fontcolor)
			}			
			title(main=list("LIM-Rule static", col=plotstyle$fontcolor), outer=TRUE)
		dev.off()		
	}
}
