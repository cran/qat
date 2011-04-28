qat_plot_roc_rule_dynamic_1d <-
function(flagvector, filename, measurement_vector=NULL, max_upward_vector=NULL, max_downward_vector=NULL, upward_vector_name=NULL, downward_vector_name=NULL, measurement_name="", directoryname="", plotstyle=NULL) {
## functionality: plot roc-dynamic-rule
## author: André Düsterhus
## date: 02.03.2010
## version: A0.1
## input: flagvector from qat_analyse_roc_rule_dynamic, measurement_vector, directoryname, filename, plotstylelist
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
			plot(flagvector, xlab="", ylab="", type="p",col.lab=plotstyle$fontcolor, col.main=plotstyle$fontcolor, col.sub=plotstyle$fontcolor, fg=plotstyle$frontcolor, col.axis=plotstyle$fontcolor, col=plotstyle$plotcolorminor, font.axis=2)
			title(main=list("ROC-Rule dynamic", col=plotstyle$fontcolor), outer=TRUE)
		dev.off()
	} else {
		png(filename=path,width=800,height=600, pointsize=12, bg=plotstyle$basecolor)
			par(font.lab=2,oma=c(0,0,2,0))
			plot(measurement_vector, col=plotstyle$plotcolormain, xlab="", ylab="", type="n", col.lab=plotstyle$fontcolor, col.main=plotstyle$fontcolor, col.sub=plotstyle$fontcolor, col.axis=plotstyle$fontcolor, pch=plotstyle$plotpointmain, fg=plotstyle$frontcolor, font.axis=2)
			if((length(max_downward_vector) != 0) && (length(max_upward_vector) != 0)) {
				index_mesvector_pre<-which(!is.na(measurement_vector))
				index_mesvector_m1<-which(!is.na(measurement_vector))-1
				index_mesvector <- intersect(index_mesvector_pre,index_mesvector_m1)
				index_upvector<- which(!is.na(max_upward_vector))
				index_downvector<- which(!is.na(max_downward_vector))
				index_upplot <- intersect((index_mesvector+1),index_upvector)
				index_downplot <- intersect((index_mesvector+1),index_downvector)	
				index_both <- intersect(index_upplot,index_downplot)			
				rect(index_both-0.5, measurement_vector[index_both-1] - max_downward_vector[index_both], index_both+0.5, measurement_vector[index_both-1] + max_upward_vector[index_both], lwd=0, border = NA,col =plotstyle$plotcolorbackground)
				points(index_upplot, measurement_vector[index_upplot-1] + max_upward_vector[index_upplot], col=plotstyle$frontcolor, pch="-")
				points(index_downplot, measurement_vector[index_downplot-1] - max_downward_vector[index_downplot], col=plotstyle$frontcolor, pch="-")
			}
			points(which(flagvector != 0), measurement_vector[which(flagvector != 0)], col=plotstyle$plotcolormain, xlab="", ylab="", type="p", col.lab=plotstyle$fontcolor, col.main=plotstyle$fontcolor, col.sub=plotstyle$fontcolor, col.axis=plotstyle$fontcolor, pch=plotstyle$plotpointmain, fg=plotstyle$frontcolor, font.axis=2)
			points(which(flagvector == 0), measurement_vector[which(flagvector == 0)], col=plotstyle$plotcolorminor, col.lab=plotstyle$fontcolor, pch=plotstyle$plotpointminor)
			title(main=list("ROC-Rule dynamic", col=plotstyle$fontcolor), outer=TRUE)
			if(length(upward_vector_name) != 0) {
				bordertext<- paste("Maximum upward vector: ", upward_vector_name, " ")
				bordertext2<- paste("Upward errors: ",length(which(flagvector==1)), " ")
			} else {
				bordertext <- ""
				bordertext2 <- ""
			}
			if(length(downward_vector_name) != 0) {
				bordertext <- paste(bordertext,"Maximum downward vector: ", downward_vector_name)
				bordertext2 <- paste(bordertext2,"Downward errors: ", length(which(flagvector==-1)))
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

