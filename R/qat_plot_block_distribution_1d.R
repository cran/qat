qat_plot_block_distribution_1d <-
function(resultlist, filename, blocksize=-1, measurement_name="", directoryname="", plotstyle=NULL) {
## functionality: plot statistical parameters of a blockwise scan of a measurement-vector
## author: André Düsterhus
## date: 23.02.2010
## version: A0.1
## input: resultlist from qat_analyse_block_distribution, directoryname, filename, plotstylelist
## output: plot
	if (is.null(plotstyle)) {
		# if no plotstyle available, use standard plotstyle
		plotstyle<-qat_style_plot()
	}
#	library("gplots")
	# set up savepath of the plot
	path <- paste(directoryname,filename,"_1.png", sep="")
	png(filename=path,width=800,height=600, pointsize=12, bg=plotstyle$basecolor)
		par(font.lab=2, mfrow=c(2,2),oma=c(0,0,2,0))
		if (length(resultlist$first_moment)!=0) {
			if(sum(is.nan(resultlist$first_moment)) != length(resultlist$first_moment)) {
				plot(resultlist$first_moment, col=plotstyle$plotcolorminor, main="1st moment", font=2, xlab="", ylab="", , type="p", col.lab=plotstyle$fontcolor, col.main=plotstyle$fontcolor, col.sub=plotstyle$fontcolor,fg=plotstyle$frontcolor, col.axis=plotstyle$fontcolor, pch=plotstyle$plotpointmain)
				abline(h=mean(resultlist$first_moment),col=plotstyle$plotcolormain)
			} else {
				textplot("no data", valign="top",col=plotstyle$fontcolor)
			} 
		}
		else {
			textplot("no data", valign="top",col=plotstyle$fontcolor)
		}
		if (length(resultlist$second_moment)!=0) {
			if(sum(is.nan(resultlist$second_moment)) != length(resultlist$second_moment)) {
				plot(resultlist$second_moment, col=plotstyle$plotcolorminor, main="2nd moment", font=2, xlab="", ylab="", type="p", col.lab=plotstyle$fontcolor, col.main=plotstyle$fontcolor, col.sub=plotstyle$fontcolor,fg=plotstyle$frontcolor, col.axis=plotstyle$fontcolor, pch=plotstyle$plotpointmain)
				abline(h=mean(resultlist$second_moment),col=plotstyle$plotcolormain)
			} else {
				textplot("no data", valign="top",col=plotstyle$fontcolor)
			}
		}
		else {
			textplot("no data", valign="top",col=plotstyle$fontcolor)
		}
		if (length(resultlist$third_moment)!=0) {
			if (sum(is.nan(resultlist$third_moment)) != length(resultlist$third_moment)) {
				plot(resultlist$third_moment, col=plotstyle$plotcolorminor, main="3rd moment", font=2, xlab="", ylab="", type="p", col.lab=plotstyle$fontcolor, col.main=plotstyle$fontcolor, col.sub=plotstyle$fontcolor,fg=plotstyle$frontcolor, col.axis=plotstyle$fontcolor, pch=plotstyle$plotpointmain)
				abline(h=mean(resultlist$third_moment),col=plotstyle$plotcolormain)
			} else {
				textplot("no data", valign="top",col=plotstyle$fontcolor)
			}
		}
		else {
			textplot("no data", valign="top",col=plotstyle$fontcolor)
		}
		if (length(resultlist$fourth_moment)!=0) {
		if(sum(is.nan(resultlist$fourth_moment)) != length(resultlist$fourth_moment))  {
			plot(resultlist$fourth_moment, col=plotstyle$plotcolorminor, main="4th moment", font=2, xlab="", ylab="", type="p", col.lab=plotstyle$fontcolor, col.main=plotstyle$fontcolor, col.sub=plotstyle$fontcolor,fg=plotstyle$frontcolor, col.axis=plotstyle$fontcolor,pch=plotstyle$plotpointmain)
			abline(h=mean(resultlist$fourth_moment),col=plotstyle$plotcolormain)
			} else {
				textplot("no data", valign="top",col=plotstyle$fontcolor)
			}
		}
		else {
			textplot("no data", valign="top",col=plotstyle$fontcolor)
		}
		mtext("Distribution of a blockwise shift (1)", side=3, line=1 ,font=2 ,col=plotstyle$fontcolor, outer=TRUE)
		if(blocksize != -1) {
			bordertext2<-paste("Blocksize: ",blocksize, sep="")
			mtext(bordertext2, side=3, line=0, font=2, col=plotstyle$fontcolor, outer=TRUE)
		}		
		if(measurement_name != "") {
			bordertext3<-paste("Data: ",measurement_name, sep="")
			mtext(bordertext3, side=3, line=-1, font=2, col=plotstyle$fontcolor, outer=TRUE)
		}	
	dev.off()
	path <- paste(directoryname,filename,"_2.png", sep="")
	png(filename=path,width=800,height=600, pointsize=12, bg=plotstyle$basecolor)
		par(font.lab=2, mfrow=c(2,2),oma=c(0,0,2,0))
		if (length(resultlist$standard_deviation)!=0) {
			if(sum(is.nan(resultlist$standard_deviation)) != length(resultlist$standard_deviation)) {
				plot(resultlist$standard_deviation, col=plotstyle$plotcolorminor, main="standard deviation", font=2, xlab="", ylab="", , type="p", col.lab=plotstyle$fontcolor, col.main=plotstyle$fontcolor, col.sub=plotstyle$fontcolor,fg=plotstyle$frontcolor, col.axis=plotstyle$fontcolor, pch=plotstyle$plotpointmain)
				abline(h=mean(resultlist$standard_deviation),col=plotstyle$plotcolormain)
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
				abline(h=mean(resultlist$skewness),col=plotstyle$plotcolormain)
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
				abline(h=mean(resultlist$kurtosis),col=plotstyle$plotcolormain)
			} else {
				textplot("no data", valign="top",col=plotstyle$fontcolor)
			} 
		}
		else {
			textplot("no data", valign="top",col=plotstyle$fontcolor)
		}
		if (length(resultlist$median)!=0) {
			if(sum(is.nan(resultlist$median)) != length(resultlist$median)) {
				plot(resultlist$median, col=plotstyle$plotcolorminor, main="median", font=2, xlab="", ylab="", type="p", col.lab=plotstyle$fontcolor, col.main=plotstyle$fontcolor, col.sub=plotstyle$fontcolor,fg=plotstyle$frontcolor, col.axis=plotstyle$fontcolor,pch=plotstyle$plotpointmain)
				abline(h=mean(resultlist$median),col=plotstyle$plotcolormain)
			} else {
				textplot("no data", valign="top",col=plotstyle$fontcolor)
			} 
		}
		else {
			textplot("no data", valign="top",col=plotstyle$fontcolor)
		}

		mtext("Distribution of a blockwise shift (2)", side=3, line=1 ,font=2 ,col=plotstyle$fontcolor, outer=TRUE)
		if(blocksize != -1) {
			bordertext2<-paste("Blocksize: ",blocksize, sep="")
			mtext(bordertext2, side=3, line=0, font=2, col=plotstyle$fontcolor, outer=TRUE)
		}		
		if(measurement_name != "") {
			bordertext3<-paste("Data: ",measurement_name, sep="")
			mtext(bordertext3, side=3, line=-1, font=2, col=plotstyle$fontcolor, outer=TRUE)
		}	
	dev.off()
	
	path <- paste(directoryname,filename,"_3.png", sep="")
	png(filename=path,width=800,height=600, pointsize=12, bg=plotstyle$basecolor)
		par(font.lab=2, mfrow=c(2,2),oma=c(0,0,2,0))
		if (length(resultlist$p5_quantile)!=0) {
			if(sum(is.nan(resultlist$p5_quantile)) != length(resultlist$p5_quantile)) {
				plot(resultlist$p5_quantile, col=plotstyle$plotcolorminor, main="5% percentile", font=2, xlab="", ylab="", , type="p", col.lab=plotstyle$fontcolor, col.main=plotstyle$fontcolor, col.sub=plotstyle$fontcolor,fg=plotstyle$frontcolor, col.axis=plotstyle$fontcolor, pch=plotstyle$plotpointmain)
				abline(h=mean(resultlist$p5_quantile),col=plotstyle$plotcolormain)
			} else {
				textplot("no data", valign="top",col=plotstyle$fontcolor)
			} 
		}
		else {
			textplot("no data", valign="top",col=plotstyle$fontcolor)
		}
		if (length(resultlist$p95_quantile)!=0) {
			if(sum(is.nan(resultlist$p95_quantile)) != length(resultlist$p95_quantile)) {
				plot(resultlist$p95_quantile, col=plotstyle$plotcolorminor, main="95% percentile", font=2, xlab="", ylab="", type="p", col.lab=plotstyle$fontcolor, col.main=plotstyle$fontcolor, col.sub=plotstyle$fontcolor,fg=plotstyle$frontcolor, col.axis=plotstyle$fontcolor, pch=plotstyle$plotpointmain)
				abline(h=mean(resultlist$p95_quantile),col=plotstyle$plotcolormain)
			} else {
				textplot("no data", valign="top",col=plotstyle$fontcolor)
			} 
		}
		else {
			textplot("no data", valign="top",col=plotstyle$fontcolor)
		}
		if (length(resultlist$p25_quantile)!=0) {
			if(sum(is.nan(resultlist$p25_quantile)) != length(resultlist$p25_quantile)) {
				plot(resultlist$p25_quantile, col=plotstyle$plotcolorminor, main="25% percentile", font=2, xlab="", ylab="", type="p", col.lab=plotstyle$fontcolor, col.main=plotstyle$fontcolor, col.sub=plotstyle$fontcolor,fg=plotstyle$frontcolor, col.axis=plotstyle$fontcolor, pch=plotstyle$plotpointmain)
				abline(h=mean(resultlist$p25_quantile),col=plotstyle$plotcolormain)
			} else {
				textplot("no data", valign="top",col=plotstyle$fontcolor)
			} 
		}
		else {
			textplot("no data", valign="top",col=plotstyle$fontcolor)
		}
		if (length(resultlist$p75_quantile)!=0) {
			if(sum(is.nan(resultlist$p75_quantile)) != length(resultlist$p75_quantile)) {
				plot(resultlist$p75_quantile, col=plotstyle$plotcolorminor, main="75% percentile", font=2, xlab="", ylab="", type="p", col.lab=plotstyle$fontcolor, col.main=plotstyle$fontcolor, col.sub=plotstyle$fontcolor,fg=plotstyle$frontcolor, col.axis=plotstyle$fontcolor,pch=plotstyle$plotpointmain)
				abline(h=mean(resultlist$p75_quantile),col=plotstyle$plotcolormain)
			} else {
				textplot("no data", valign="top",col=plotstyle$fontcolor)
			} 
		}
		else {
			textplot("no data", valign="top",col=plotstyle$fontcolor)
		}
		mtext("Distribution of a blockwise shift (3)", side=3, line=1 ,font=2 ,col=plotstyle$fontcolor, outer=TRUE)
		if(blocksize != -1) {
			bordertext2<-paste("Blocksize: ",blocksize, sep="")
			mtext(bordertext2, side=3, line=0, font=2, col=plotstyle$fontcolor, outer=TRUE)
		}		
		if(measurement_name != "") {
			bordertext3<-paste("Data: ",measurement_name, sep="")
			mtext(bordertext3, side=3, line=-1, font=2, col=plotstyle$fontcolor, outer=TRUE)
		}	
	dev.off()

}
