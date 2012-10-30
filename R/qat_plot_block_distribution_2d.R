qat_plot_block_distribution_2d <-
function(resultlist, filename, blocksize=-1, measurement_name="", directoryname="", plotstyle=NULL) {
## functionality: plot statistical parameters of a blockwise scan of a measurement-vector
## author: André Düsterhus
## date: 16.10.2012
## version: A0.1
## input: resultlist from qat_analyse_block_distribution, directoryname, filename, plotstylelist
## output: plot
	if (is.null(plotstyle)) {
		# if no plotstyle available, use standard plotstyle
		plotstyle<-qat_style_plot()
	}
	library("gplots")
	library("fields")
	# set up savepath of the plot
	path <- paste(directoryname,filename,"_1.png", sep="")
	png(filename=path,width=800,height=600, pointsize=12, bg=plotstyle$basecolor)
		par(font.lab=2,oma=c(0,0,2,0), mar=c(5.1,5.1,5.1,8), font=2, mfrow=c(2,2))
		if (length(dim(resultlist$first_moment))==2) {
			image(1:dim(resultlist$first_moment)[1], 1:dim(resultlist$first_moment)[2], resultlist$first_moment, col=tim.colors(64), xlab="", ylab="", col.lab=plotstyle$fontcolor, col.main=plotstyle$fontcolor, col.sub=plotstyle$fontcolor,fg=plotstyle$frontcolor, col.axis=plotstyle$fontcolor, font.axis=2, main="first moment")
			mtext("Distribution of a blockwise shift (1)", side=3, line=1 ,font=2 ,col=plotstyle$fontcolor, outer=TRUE)
			if(blocksize != -1) {
				bordertext2<-paste("Blocksize: ",blocksize, sep="")
				mtext(bordertext2, side=3, line=0, font=2, col=plotstyle$fontcolor, outer=TRUE)
			}		
			if(measurement_name != "") {
				bordertext3<-paste("Data: ",measurement_name, sep="")
				mtext(bordertext3, side=3, line=-1, font=2, col=plotstyle$fontcolor, outer=TRUE)
			}	
			image.plot(1:dim(resultlist$first_moment)[1], 1:dim(resultlist$first_moment)[2], resultlist$first_moment, col=tim.colors(64), xlab="", ylab="", col.lab=plotstyle$fontcolor, col.main=plotstyle$fontcolor, col.sub=plotstyle$fontcolor,fg=plotstyle$frontcolor, col.axis=plotstyle$fontcolor, font.axis=2, legend.only=T)	
		} else {
			textplot("no data", valign="top",col=plotstyle$fontcolor)
		}
		if (length(dim(resultlist$second_moment))==2) {
			image(1:dim(resultlist$second_moment)[1], 1:dim(resultlist$second_moment)[2], resultlist$second_moment, col=tim.colors(64), xlab="", ylab="", col.lab=plotstyle$fontcolor, col.main=plotstyle$fontcolor, col.sub=plotstyle$fontcolor,fg=plotstyle$frontcolor, col.axis=plotstyle$fontcolor, font.axis=2, main="second moment")
			image.plot(1:dim(resultlist$second_moment)[1], 1:dim(resultlist$second_moment)[2], resultlist$second_moment, col=tim.colors(64), xlab="", ylab="", col.lab=plotstyle$fontcolor, col.main=plotstyle$fontcolor, col.sub=plotstyle$fontcolor,fg=plotstyle$frontcolor, col.axis=plotstyle$fontcolor, font.axis=2, legend.only=T)	
		} else {
			textplot("no data", valign="top",col=plotstyle$fontcolor)
		}
		if (length(dim(resultlist$third_moment))==2) {
			image(1:dim(resultlist$third_moment)[1], 1:dim(resultlist$third_moment)[2], resultlist$third_moment, col=tim.colors(64), xlab="", ylab="", col.lab=plotstyle$fontcolor, col.main=plotstyle$fontcolor, col.sub=plotstyle$fontcolor,fg=plotstyle$frontcolor, col.axis=plotstyle$fontcolor, font.axis=2, main="third moment")
			image.plot(1:dim(resultlist$third_moment)[1], 1:dim(resultlist$third_moment)[2], resultlist$third_moment, col=tim.colors(64), xlab="", ylab="", col.lab=plotstyle$fontcolor, col.main=plotstyle$fontcolor, col.sub=plotstyle$fontcolor,fg=plotstyle$frontcolor, col.axis=plotstyle$fontcolor, font.axis=2, legend.only=T)	
		} else {
			textplot("no data", valign="top",col=plotstyle$fontcolor)
		}
		if (length(dim(resultlist$fourth_moment))==2) {
			image(1:dim(resultlist$fourth_moment)[1], 1:dim(resultlist$fourth_moment)[2], resultlist$fourth_moment, col=tim.colors(64), xlab="", ylab="", col.lab=plotstyle$fontcolor, col.main=plotstyle$fontcolor, col.sub=plotstyle$fontcolor,fg=plotstyle$frontcolor, col.axis=plotstyle$fontcolor, font.axis=2, main="fourth moment")
			image.plot(1:dim(resultlist$fourth_moment)[1], 1:dim(resultlist$fourth_moment)[2], resultlist$fourth_moment, col=tim.colors(64), xlab="", ylab="", col.lab=plotstyle$fontcolor, col.main=plotstyle$fontcolor, col.sub=plotstyle$fontcolor,fg=plotstyle$frontcolor, col.axis=plotstyle$fontcolor, font.axis=2, legend.only=T)	
		} else {
			textplot("no data", valign="top",col=plotstyle$fontcolor)
		}
	dev.off()
	path <- paste(directoryname,filename,"_2.png", sep="")
	png(filename=path,width=800,height=600, pointsize=12, bg=plotstyle$basecolor)
		par(font.lab=2,oma=c(0,0,2,0), mar=c(5.1,5.1,5.1,8), font=2, mfrow=c(2,2))
		if (length(dim(resultlist$standard_deviation))==2) {
			image(1:dim(resultlist$standard_deviation)[1], 1:dim(resultlist$standard_deviation)[2], resultlist$standard_deviation, col=tim.colors(64), xlab="", ylab="", col.lab=plotstyle$fontcolor, col.main=plotstyle$fontcolor, col.sub=plotstyle$fontcolor,fg=plotstyle$frontcolor, col.axis=plotstyle$fontcolor, font.axis=2, main="standard deviation")
			mtext("Distribution of a blockwise shift (2)", side=3, line=1 ,font=2 ,col=plotstyle$fontcolor, outer=TRUE)
			if(blocksize != -1) {
				bordertext2<-paste("Blocksize: ",blocksize, sep="")
				mtext(bordertext2, side=3, line=0, font=2, col=plotstyle$fontcolor, outer=TRUE)
			}		
			if(measurement_name != "") {
				bordertext3<-paste("Data: ",measurement_name, sep="")
				mtext(bordertext3, side=3, line=-1, font=2, col=plotstyle$fontcolor, outer=TRUE)
			}	
			image.plot(1:dim(resultlist$standard_deviation)[1], 1:dim(resultlist$standard_deviation)[2], resultlist$standard_deviation, col=tim.colors(64), xlab="", ylab="", col.lab=plotstyle$fontcolor, col.main=plotstyle$fontcolor, col.sub=plotstyle$fontcolor,fg=plotstyle$frontcolor, col.axis=plotstyle$fontcolor, font.axis=2, legend.only=T)	
		} else {
			textplot("no data", valign="top",col=plotstyle$fontcolor)
		}
		if (length(dim(resultlist$skewness))==2) {
			image(1:dim(resultlist$skewness)[1], 1:dim(resultlist$skewness)[2], resultlist$skewness, col=tim.colors(64), xlab="", ylab="", col.lab=plotstyle$fontcolor, col.main=plotstyle$fontcolor, col.sub=plotstyle$fontcolor,fg=plotstyle$frontcolor, col.axis=plotstyle$fontcolor, font.axis=2, main="skewness")
			image.plot(1:dim(resultlist$skewness)[1], 1:dim(resultlist$skewness)[2], resultlist$skewness, col=tim.colors(64), xlab="", ylab="", col.lab=plotstyle$fontcolor, col.main=plotstyle$fontcolor, col.sub=plotstyle$fontcolor,fg=plotstyle$frontcolor, col.axis=plotstyle$fontcolor, font.axis=2, legend.only=T)	
		} else {
			textplot("no data", valign="top",col=plotstyle$fontcolor)
		}
		if (length(dim(resultlist$kurtosis))==2) {
			image(1:dim(resultlist$kurtosis)[1], 1:dim(resultlist$kurtosis)[2], resultlist$kurtosis, col=tim.colors(64), xlab="", ylab="", col.lab=plotstyle$fontcolor, col.main=plotstyle$fontcolor, col.sub=plotstyle$fontcolor,fg=plotstyle$frontcolor, col.axis=plotstyle$fontcolor, font.axis=2, main="kurtosis")
			image.plot(1:dim(resultlist$kurtosis)[1], 1:dim(resultlist$kurtosis)[2], resultlist$kurtosis, col=tim.colors(64), xlab="", ylab="", col.lab=plotstyle$fontcolor, col.main=plotstyle$fontcolor, col.sub=plotstyle$fontcolor,fg=plotstyle$frontcolor, col.axis=plotstyle$fontcolor, font.axis=2, legend.only=T)	
		} else {
			textplot("no data", valign="top",col=plotstyle$fontcolor)
		}
		if (length(dim(resultlist$median))==2) {
			image(1:dim(resultlist$median)[1], 1:dim(resultlist$median)[2], resultlist$median, col=tim.colors(64), xlab="", ylab="", col.lab=plotstyle$fontcolor, col.main=plotstyle$fontcolor, col.sub=plotstyle$fontcolor,fg=plotstyle$frontcolor, col.axis=plotstyle$fontcolor, font.axis=2, main="median")
			image.plot(1:dim(resultlist$median)[1], 1:dim(resultlist$median)[2], resultlist$median, col=tim.colors(64), xlab="", ylab="", col.lab=plotstyle$fontcolor, col.main=plotstyle$fontcolor, col.sub=plotstyle$fontcolor,fg=plotstyle$frontcolor, col.axis=plotstyle$fontcolor, font.axis=2, legend.only=T)	
		} else {
			textplot("no data", valign="top",col=plotstyle$fontcolor)
		}
	dev.off()
	path <- paste(directoryname,filename,"_3.png", sep="")
	png(filename=path,width=800,height=600, pointsize=12, bg=plotstyle$basecolor)
		par(font.lab=2,oma=c(0,0,2,0), mar=c(5.1,5.1,5.1,8), font=2, mfrow=c(2,2))
		if (length(dim(resultlist$p5_quantile))==2) {
			image(1:dim(resultlist$p5_quantile)[1], 1:dim(resultlist$p5_quantile)[2], resultlist$p5_quantile, col=tim.colors(64), xlab="", ylab="", col.lab=plotstyle$fontcolor, col.main=plotstyle$fontcolor, col.sub=plotstyle$fontcolor,fg=plotstyle$frontcolor, col.axis=plotstyle$fontcolor, font.axis=2, main="5% percentile")
			mtext("Distribution of a blockwise shift (3)", side=3, line=1 ,font=2 ,col=plotstyle$fontcolor, outer=TRUE)
			if(blocksize != -1) {
				bordertext2<-paste("Blocksize: ",blocksize, sep="")
				mtext(bordertext2, side=3, line=0, font=2, col=plotstyle$fontcolor, outer=TRUE)
			}		
			if(measurement_name != "") {
				bordertext3<-paste("Data: ",measurement_name, sep="")
				mtext(bordertext3, side=3, line=-1, font=2, col=plotstyle$fontcolor, outer=TRUE)
			}	
			image.plot(1:dim(resultlist$p5_quantile)[1], 1:dim(resultlist$p5_quantile)[2], resultlist$p5_quantile, col=tim.colors(64), xlab="", ylab="", col.lab=plotstyle$fontcolor, col.main=plotstyle$fontcolor, col.sub=plotstyle$fontcolor,fg=plotstyle$frontcolor, col.axis=plotstyle$fontcolor, font.axis=2, legend.only=T)	
		} else {
			textplot("no data", valign="top",col=plotstyle$fontcolor)
		}
		if (length(dim(resultlist$p95_quantile))==2) {
			image(1:dim(resultlist$p95_quantile)[1], 1:dim(resultlist$p95_quantile)[2], resultlist$p95_quantile, col=tim.colors(64), xlab="", ylab="", col.lab=plotstyle$fontcolor, col.main=plotstyle$fontcolor, col.sub=plotstyle$fontcolor,fg=plotstyle$frontcolor, col.axis=plotstyle$fontcolor, font.axis=2, main="95% percentile")
			image.plot(1:dim(resultlist$p95_quantile)[1], 1:dim(resultlist$p95_quantile)[2], resultlist$p95_quantile, col=tim.colors(64), xlab="", ylab="", col.lab=plotstyle$fontcolor, col.main=plotstyle$fontcolor, col.sub=plotstyle$fontcolor,fg=plotstyle$frontcolor, col.axis=plotstyle$fontcolor, font.axis=2, legend.only=T)	
		} else {
			textplot("no data", valign="top",col=plotstyle$fontcolor)
		}
		if (length(dim(resultlist$p25_quantile))==2) {
			image(1:dim(resultlist$p25_quantile)[1], 1:dim(resultlist$p25_quantile)[2], resultlist$p25_quantile, col=tim.colors(64), xlab="", ylab="", col.lab=plotstyle$fontcolor, col.main=plotstyle$fontcolor, col.sub=plotstyle$fontcolor,fg=plotstyle$frontcolor, col.axis=plotstyle$fontcolor, font.axis=2, main="25% percentile")
			image.plot(1:dim(resultlist$p25_quantile)[1], 1:dim(resultlist$p25_quantile)[2], resultlist$p25_quantile, col=tim.colors(64), xlab="", ylab="", col.lab=plotstyle$fontcolor, col.main=plotstyle$fontcolor, col.sub=plotstyle$fontcolor,fg=plotstyle$frontcolor, col.axis=plotstyle$fontcolor, font.axis=2, legend.only=T)	
		} else {
			textplot("no data", valign="top",col=plotstyle$fontcolor)
		}
		if (length(dim(resultlist$p75_quantile))==2) {
			image(1:dim(resultlist$p75_quantile)[1], 1:dim(resultlist$p75_quantile)[2], resultlist$p75_quantile, col=tim.colors(64), xlab="", ylab="", col.lab=plotstyle$fontcolor, col.main=plotstyle$fontcolor, col.sub=plotstyle$fontcolor,fg=plotstyle$frontcolor, col.axis=plotstyle$fontcolor, font.axis=2, main="75% percentile")
			image.plot(1:dim(resultlist$p75_quantile)[1], 1:dim(resultlist$p75_quantile)[2], resultlist$p75_quantile, col=tim.colors(64), xlab="", ylab="", col.lab=plotstyle$fontcolor, col.main=plotstyle$fontcolor, col.sub=plotstyle$fontcolor,fg=plotstyle$frontcolor, col.axis=plotstyle$fontcolor, font.axis=2, legend.only=T)	
		} else {
			textplot("no data", valign="top",col=plotstyle$fontcolor)
		}
	dev.off()
}
