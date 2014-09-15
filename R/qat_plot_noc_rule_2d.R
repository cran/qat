qat_plot_noc_rule_2d <-
function(flagvector, filename, measurement_vector=NULL, max_return_elements=0, measurement_name="", directoryname="", plotstyle=NULL) {
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
#	library("fields")
	# set up savepath of the plot
	path <- paste(directoryname,filename,".png", sep="")
	png(filename=path,width=800,height=600, pointsize=12, bg=plotstyle$basecolor)
		par(font.lab=2,oma=c(0,0,2,0), mar=c(5.1,5.1,5.1,8), font=2)
		image(1:dim(flagvector)[1], 1:dim(flagvector)[2], flagvector, col=c(plotstyle$plotcolorbackground,plotstyle$plotcolormain), xlab="", ylab="", col.lab=plotstyle$fontcolor, col.main=plotstyle$fontcolor, col.sub=plotstyle$fontcolor,fg=plotstyle$frontcolor, col.axis=plotstyle$fontcolor, font.axis=2, zlim=c(0,1))
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
		image.plot(1:dim(flagvector)[1], 1:dim(flagvector)[2], flagvector, col=c(plotstyle$plotcolorbackground,plotstyle$plotcolormain), xlab="", ylab="", col.lab=plotstyle$fontcolor, col.main=plotstyle$fontcolor, col.sub=plotstyle$fontcolor,fg=plotstyle$frontcolor, col.axis=plotstyle$fontcolor, font.axis=2, legend.only=T, axis.args=list(at=c(0,1), labels=c("ok", "error"), font=2), zlim=c(0,1))
	dev.off()		
}
