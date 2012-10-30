qat_plot_trimmed_distribution_1d <-
function(resultlist, filename, measurement_name="", directoryname="",plotstyle=NULL) {
## functionality: plot trimmed moments
## author: André Düsterhus
## date: 23.02.2010
## version: A0.1
## input: resultlist from qat_analyse_trimmed_distribution, directoryname, filename, plotstylelist
## output: plot
	if (is.null(plotstyle)) {
		# if no plotstyle available, use standard plotstyle
		plotstyle<-qat_style_plot()
	}
	library("gplots")
	# set up savepath of the plot
	path <- paste(directoryname,filename,".png", sep="")
	png(filename=path,width=800,height=600, pointsize=12, bg=plotstyle$basecolor)
		par(font.lab=2, mfrow=c(2,2),oma=c(0,0,2,0))
		if (length(resultlist$first_moment)!=0) {
			if(sum(is.nan(resultlist$first_moment)) != length(resultlist$first_moment)) {
				plot(resultlist$first_moment, col=plotstyle$plotcolorminor, main="mean", font=2, xlab="", ylab="", type="p", col.lab=plotstyle$fontcolor, col.main=plotstyle$fontcolor, col.sub=plotstyle$fontcolor,fg=plotstyle$frontcolor, col.axis=plotstyle$fontcolor, pch=plotstyle$plotpointmain)
			} else {
				textplot("no data", valign="top",col=plotstyle$fontcolor)
			} 
		}
		else {
			textplot("no data", valign="top",col=plotstyle$fontcolor)
		}
		if (length(resultlist$standard_deviation)!=0) {
			if(sum(is.nan(resultlist$standard_deviation)) != length(resultlist$standard_deviation)) {
				plot(resultlist$standard_deviation, col=plotstyle$plotcolorminor, main="standard deviation", font=2, xlab="", ylab="", type="p", col.lab=plotstyle$fontcolor, col.main=plotstyle$fontcolor, col.sub=plotstyle$fontcolor,fg=plotstyle$frontcolor, col.axis=plotstyle$fontcolor, pch=plotstyle$plotpointmain)
			} else {
				textplot("no data", valign="top",col=plotstyle$fontcolor)
			} 
		}
		else {
			textplot("no data", valign="top",col=plotstyle$fontcolor)
		}
		if (length(resultlist$skewness)!=0) {
			if(sum(is.nan(resultlist$skewness)) != length(resultlist$skewness)) {
				plot(resultlist$skewness, col=plotstyle$plotcolorminor, main="skewness", font=2, xlab="", ylab="", type="p", col.lab=plotstyle$fontcolor, col.main=plotstyle$fontcolor, col.sub=plotstyle$fontcolor,fg=plotstyle$frontcolor, col.axis=plotstyle$fontcolor, pch=plotstyle$plotpointmain)
			} else {
				textplot("no data", valign="top",col=plotstyle$fontcolor)
			} 
		}
		else {
			textplot("no data", valign="top",col=plotstyle$fontcolor)
		}
		if (length(resultlist$kurtosis)!=0) {
			if(sum(is.nan(resultlist$kurtosis)) != length(resultlist$kurtosis)) {
				plot(resultlist$kurtosis, col=plotstyle$plotcolorminor, main="kurtosis", font=2, xlab="", ylab="", type="p", col.lab=plotstyle$fontcolor, col.main=plotstyle$fontcolor, col.sub=plotstyle$fontcolor,fg=plotstyle$frontcolor, col.axis=plotstyle$fontcolor, pch=plotstyle$plotpointmain)
			} else {
				textplot("no data", valign="top",col=plotstyle$fontcolor)
			} 
		}
		else {
			textplot("no data", valign="top",col=plotstyle$fontcolor)
		}
		mtext("Trimmed Moments", side=3, line=1 ,font=2 ,col=plotstyle$fontcolor, outer=TRUE)	
		if(measurement_name != "") {
			bordertext3<-paste("Data: ",measurement_name, sep="")
			mtext(bordertext3, side=3, line=0, font=2, col=plotstyle$fontcolor, outer=TRUE)
		}
	dev.off()	
}
