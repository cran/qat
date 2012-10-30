qat_plot_lim_rule_dynamic_2d <-
function(flagvector, filename, measurement_vector=NULL, min_vector=NULL, max_vector=NULL, min_vector_name=NULL, max_vector_name=NULL, measurement_name="", directoryname="", plotstyle=NULL) {
## functionality: plot lim-static-rule
## author: André Düsterhus
## date: 03.08.2011
## version: A0.1
## input: flagvector from qat_analyse_lim_rule_static, measurement_vector, min_value, max_value, directoryname, filename, plotstylelist
## output: plot
	if (is.null(plotstyle)) {
		# if no plotstyle available, use standard plotstyle
		plotstyle<-qat_style_plot()
	}
	library("fields")
	# set up savepath of the plot
	path <- paste(directoryname,filename,".png", sep="")
	png(filename=path,width=800,height=600, pointsize=12, bg=plotstyle$basecolor)
		par(font.lab=2,oma=c(0,0,2,0), mar=c(5.1,5.1,5.1,8), font=2)
		image(1:dim(flagvector)[1], 1:dim(flagvector)[2], flagvector, col=c(plotstyle$plotcolormain,plotstyle$plotcolorbackground,plotstyle$plotcolorminor), xlab="", ylab="", col.lab=plotstyle$fontcolor, col.main=plotstyle$fontcolor, col.sub=plotstyle$fontcolor,fg=plotstyle$frontcolor, col.axis=plotstyle$fontcolor, font.axis=2, zlim=c(-1,1))
		title(main=list("LIM-Rule dynamic", col=plotstyle$fontcolor), outer=TRUE)
		# information text at the top of the plot
		if (length(min_vector_name) != 0) {
			bordertext <- paste("Minimum vector: ", min_vector_name, " ")
		} else {
			bordertext<- ""
		}
		bordertext2<- paste("Minimum errors: ",length(which(flagvector==-1)), " ")
		bordertext2<- paste(bordertext2,"Maximum errors: ",length(which(flagvector==1)))

		if (length(max_vector_name) != 0) {
			bordertext <- paste(bordertext,"Maximum vector: ", max_vector_name)
		} else {
			bordertext<- paste(bordertext,"")
		}
		if (bordertext != "") {
			mtext(bordertext, side=3, line=2, font=2, col=plotstyle$fontcolor)
			mtext(bordertext2, side=3, line=1, font=2, col=plotstyle$fontcolor)
		}
		if(measurement_name != "") {
			bordertext3<-paste("Data: ",measurement_name, sep="")
			mtext(bordertext3, side=3, line=3, font=2, col=plotstyle$fontcolor)
		}
		image.plot(1:dim(flagvector)[1], 1:dim(flagvector)[2], flagvector, col=c(plotstyle$plotcolormain,plotstyle$plotcolorbackground,plotstyle$plotcolorminor), xlab="", ylab="", col.lab=plotstyle$fontcolor, col.main=plotstyle$fontcolor, col.sub=plotstyle$fontcolor,fg=plotstyle$frontcolor, col.axis=plotstyle$fontcolor, font.axis=2, legend.only=T, axis.args=list(at=c(-1,0,1), labels=c("min", "ok", "max"), font=2), zlim=c(-1,1))
	dev.off()		
}
