qat_plot_roc_rule_static_1d <-
function(flagvector, filename, measurement_vector=NULL, max_upward_value=0, max_downward_value=0, measurement_name="", directoryname="", plotstyle=NULL) {
## functionality: plot roc-static-rule
## author: André Düsterhus
## date: 24.02.2010
## version: A0.1
## input: flagvector from qat_analyse_roc_rule_static, measurement_vector, directoryname, filename, plotstylelist
## output: plot
	if (is.null(plotstyle)) {
		# if no plotstyle available, use standard plotstyle
		plotstyle<-qat_style_plot()
	}
	# set up savepath of the plot
	if (length(measurement_vector) == 0 || (sum(is.nan(measurement_vector))==length(measurement_vector))) {
		path <- paste(directoryname,filename,".png", sep="")
		png(filename=path,width=800,height=600, pointsize=12, bg=plotstyle$basecolor)
			par(font.lab=2,oma=c(0,0,2,0))
			plot(flagvector, col=plotstyle$plotcolorminor, xlab="", ylab="", type="p", col.lab=plotstyle$fontcolor, col.main=plotstyle$fontcolor, col.sub=plotstyle$fontcolor, fg=plotstyle$frontcolor, col.axis=plotstyle$fontcolor, font.axis=2)
			title(main=list("ROC-Rule static", col=plotstyle$fontcolor), outer=TRUE)
		dev.off()
	} else {
		path <- paste(directoryname,filename,".png", sep="")
		png(filename=path,width=800,height=600, pointsize=12, bg=plotstyle$basecolor)
			par(font.lab=2,oma=c(0,0,2,0))
			plot(measurement_vector, col=plotstyle$plotcolormain, xlab="", ylab="", type="n", col.lab=plotstyle$fontcolor, col.main=plotstyle$fontcolor, col.sub=plotstyle$fontcolor,fg=plotstyle$frontcolor, col.axis=plotstyle$fontcolor, pch=plotstyle$plotpointmain, font.axis=2)
			if((max_downward_value != 0) && (max_upward_value != 0)) {
				index_mesvector_pre<-which(!is.na(measurement_vector))
				index_mesvector_m1<-which(!is.na(measurement_vector))-1
				index_mesvector <- intersect(index_mesvector_pre,index_mesvector_m1)
				rect(index_mesvector-0.5+1, measurement_vector[index_mesvector] - max_downward_value, index_mesvector +0.5+1, measurement_vector[index_mesvector] + max_upward_value, lwd=0, border = NA,col =plotstyle$plotcolorbackground)
				points(index_mesvector+1, measurement_vector[index_mesvector] - max_downward_value, col=plotstyle$frontcolor, pch="-")
				points(index_mesvector+1, measurement_vector[index_mesvector] + max_upward_value, col=plotstyle$frontcolor, pch="-")
			}
			points(which(flagvector != 0), measurement_vector[which(flagvector != 0)], col=plotstyle$plotcolormain, xlab="", ylab="", type="p", col.lab=plotstyle$fontcolor, col.main=plotstyle$fontcolor, col.sub=plotstyle$fontcolor,fg=plotstyle$frontcolor, col.axis=plotstyle$fontcolor, pch=plotstyle$plotpointmain, font.axis=2)
			points(which(flagvector == 0), measurement_vector[which(flagvector == 0)], col=plotstyle$plotcolorminor, col.lab=plotstyle$fontcolor, pch=plotstyle$plotpointminor)
			title(main=list("ROC-Rule static", col=plotstyle$fontcolor), outer=TRUE)
			if(max_upward_value != 0) {
				bordertext<- paste("Maximum upward value: ", max_upward_value, " ")
				bordertext2<- paste("Upward errors: ",length(which(flagvector==1)), " ")
			} else {
				bordertext <- ""
				bordertext2 <- ""

			}
			if(max_downward_value != 0) {
				bordertext <- paste(bordertext,"Maximum downward value: ", max_downward_value)
				bordertext2 <- paste(bordertext2,"Downward errors: ", length(which(flagvector==-1)))
			} else {
				bordertext <- paste(bordertext, "")
				bordertext2 <- paste(bordertext2, "")				
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
