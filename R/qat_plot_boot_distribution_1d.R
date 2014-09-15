qat_plot_boot_distribution_1d <-
function(resultlist_stat, filename, bootruns=-1, measurement_name="", directoryname="",plotstyle=NULL) {
## functionality: plot bootstrapped moments
## author: André Düsterhus
## date: 06.06.2010
## version: A0.1
## input: resultlist from qat_analyse_boot_distribution, directoryname, filename, plotstylelist
## output: plot
	if (is.null(plotstyle)) {
		# if no plotstyle available, use standard plotstyle
		plotstyle<-qat_style_plot()
	}
#	library("gplots")
	# set up savepath of the plot
	path <- paste(directoryname,filename,".png", sep="")
	png(filename=path,width=800,height=600, pointsize=12, bg=plotstyle$basecolor)
		par(font.lab=2, mfrow=c(2,6),oma=c(0,0,2,0))

		if (length(resultlist_stat$first_moment)!=0) {
			if(sum(is.nan(resultlist_stat$first_moment)) != length(resultlist_stat$first_moment)) {
				boxplot(resultlist_stat$first_moment, main="mean", col=plotstyle$plotcolorbackground, font=2, xlab="", ylab="", col.lab=plotstyle$fontcolor, col.main=plotstyle$fontcolor, col.sub=plotstyle$fontcolor,fg=plotstyle$frontcolor, col.axis=plotstyle$fontcolor, pch=plotstyle$plotpointminor)
			} else {
				textplot("no data", valign="top",col=plotstyle$fontcolor)
			} 
		}
		else {
			textplot("no data", valign="top",col=plotstyle$fontcolor)
		}
		if (length(resultlist_stat$second_moment)!=0) {
			if(sum(is.nan(resultlist_stat$second_moment)) != length(resultlist_stat$second_moment)) {
				boxplot(resultlist_stat$second_moment, main="2nd moment", col=plotstyle$plotcolorbackground, font=2, xlab="", ylab="", col.lab=plotstyle$fontcolor, col.main=plotstyle$fontcolor, col.sub=plotstyle$fontcolor,fg=plotstyle$frontcolor, col.axis=plotstyle$fontcolor, pch=plotstyle$plotpointminor)
			} else {
				textplot("no data", valign="top",col=plotstyle$fontcolor)
			} 
		}
		else {
			textplot("no data", valign="top",col=plotstyle$fontcolor)
		}
		if (length(resultlist_stat$third_moment)!=0) {
			if(sum(is.nan(resultlist_stat$third_moment)) != length(resultlist_stat$third_moment)) {
				boxplot(resultlist_stat$third_moment, main="3rd moment", col=plotstyle$plotcolorbackground, font=2, xlab="", ylab="", col.lab=plotstyle$fontcolor, col.main=plotstyle$fontcolor, col.sub=plotstyle$fontcolor,fg=plotstyle$frontcolor, col.axis=plotstyle$fontcolor, pch=plotstyle$plotpointminor)
			} else {
				textplot("no data", valign="top",col=plotstyle$fontcolor)
			} 
		}
		else {
			textplot("no data", valign="top",col=plotstyle$fontcolor)
		}
		if (length(resultlist_stat$fourth_moment)!=0) {
			if(sum(is.nan(resultlist_stat$fourth_moment)) != length(resultlist_stat$fourth_moment)) {
				boxplot(resultlist_stat$fourth_moment, main="4th moment", col=plotstyle$plotcolorbackground, font=2, xlab="", ylab="", col.lab=plotstyle$fontcolor, col.main=plotstyle$fontcolor, col.sub=plotstyle$fontcolor,fg=plotstyle$frontcolor, col.axis=plotstyle$fontcolor, pch=plotstyle$plotpointminor)
			} else {
				textplot("no data", valign="top",col=plotstyle$fontcolor)
			} 
		}
		else {
			textplot("no data", valign="top",col=plotstyle$fontcolor)
		}
		if (length(resultlist_stat$standard_deviation)!=0) {
			if(sum(is.nan(resultlist_stat$standard_deviation)) != length(resultlist_stat$standard_deviation)) {
				boxplot(resultlist_stat$standard_deviation, main="std. dev.", col=plotstyle$plotcolorbackground, font=2, xlab="", ylab="", col.lab=plotstyle$fontcolor, col.main=plotstyle$fontcolor, col.sub=plotstyle$fontcolor,fg=plotstyle$frontcolor, col.axis=plotstyle$fontcolor, pch=plotstyle$plotpointminor)
			} else {
				textplot("no data", valign="top",col=plotstyle$fontcolor)
			} 
		}
		else {
			textplot("no data", valign="top",col=plotstyle$fontcolor)
		}
		if (length(resultlist_stat$skewness)!=0) {
			if(sum(is.nan(resultlist_stat$skewness)) != length(resultlist_stat$skewness)) {
				boxplot(resultlist_stat$skewness, main="skewness", col=plotstyle$plotcolorbackground, font=2, xlab="", ylab="", col.lab=plotstyle$fontcolor, col.main=plotstyle$fontcolor, col.sub=plotstyle$fontcolor,fg=plotstyle$frontcolor, col.axis=plotstyle$fontcolor, pch=plotstyle$plotpointminor)
			} else {
				textplot("no data", valign="top",col=plotstyle$fontcolor)
			} 
		}
		else {
			textplot("no data", valign="top",col=plotstyle$fontcolor)
		}
		if (length(resultlist_stat$kurtosis)!=0) {
			if(sum(is.nan(resultlist_stat$kurtosis)) != length(resultlist_stat$kurtosis)) {
				boxplot(resultlist_stat$kurtosis, main="kurtosis", col=plotstyle$plotcolorbackground, font=2, xlab="", ylab="", col.lab=plotstyle$fontcolor, col.main=plotstyle$fontcolor, col.sub=plotstyle$fontcolor,fg=plotstyle$frontcolor, col.axis=plotstyle$fontcolor, pch=plotstyle$plotpointminor)
			} else {
				textplot("no data", valign="top",col=plotstyle$fontcolor)
			} 
		}
		else {
			textplot("no data", valign="top",col=plotstyle$fontcolor)
		}
		if (length(resultlist_stat$median)!=0) {
			if(sum(is.nan(resultlist_stat$median)) != length(resultlist_stat$median)) {
				boxplot(resultlist_stat$median, main="median", col=plotstyle$plotcolorbackground, font=2, xlab="", ylab="", col.lab=plotstyle$fontcolor, col.main=plotstyle$fontcolor, col.sub=plotstyle$fontcolor,fg=plotstyle$frontcolor, col.axis=plotstyle$fontcolor, pch=plotstyle$plotpointminor)
			} else {
				textplot("no data", valign="top",col=plotstyle$fontcolor)
			} 
		}
		else {
			textplot("no data", valign="top",col=plotstyle$fontcolor)
		}
		if (length(resultlist_stat$p5_quantile)!=0) {
			if(sum(is.nan(resultlist_stat$p5_quantile)) != length(resultlist_stat$p5_quantile)) {
				boxplot(resultlist_stat$p5_quantile, main="5% quantile", col=plotstyle$plotcolorbackground, font=2, xlab="", ylab="", col.lab=plotstyle$fontcolor, col.main=plotstyle$fontcolor, col.sub=plotstyle$fontcolor,fg=plotstyle$frontcolor, col.axis=plotstyle$fontcolor, pch=plotstyle$plotpointminor)
			} else {
				textplot("no data", valign="top",col=plotstyle$fontcolor)
			} 
		}
		else {
			textplot("no data", valign="top",col=plotstyle$fontcolor)
		}
		if (length(resultlist_stat$p25_quantile)!=0) {
			if(sum(is.nan(resultlist_stat$p25_quantile)) != length(resultlist_stat$p25_quantile)) {
				boxplot(resultlist_stat$p25_quantile, main="25% quantile", col=plotstyle$plotcolorbackground, font=2, xlab="", ylab="", col.lab=plotstyle$fontcolor, col.main=plotstyle$fontcolor, col.sub=plotstyle$fontcolor,fg=plotstyle$frontcolor, col.axis=plotstyle$fontcolor, pch=plotstyle$plotpointminor)
			} else {
				textplot("no data", valign="top",col=plotstyle$fontcolor)
			} 
		}
		else {
			textplot("no data", valign="top",col=plotstyle$fontcolor)
		}
		if (length(resultlist_stat$p75_quantile)!=0) {
			if(sum(is.nan(resultlist_stat$p75_quantile)) != length(resultlist_stat$p75_quantile)) {
				boxplot(resultlist_stat$p75_quantile, main="75% quantile", col=plotstyle$plotcolorbackground, font=2, xlab="", ylab="", col.lab=plotstyle$fontcolor, col.main=plotstyle$fontcolor, col.sub=plotstyle$fontcolor,fg=plotstyle$frontcolor, col.axis=plotstyle$fontcolor, pch=plotstyle$plotpointminor)
			} else {
				textplot("no data", valign="top",col=plotstyle$fontcolor)
			} 
		}
		else {
			textplot("no data", valign="top",col=plotstyle$fontcolor)
		}
		if (length(resultlist_stat$p95_quantile)!=0) {
			if(sum(is.nan(resultlist_stat$p95_quantile)) != length(resultlist_stat$p95_quantile)) {
				boxplot(resultlist_stat$p95_quantile, main="95% quantile", col=plotstyle$plotcolorbackground, font=2, xlab="", ylab="", col.lab=plotstyle$fontcolor, col.main=plotstyle$fontcolor, col.sub=plotstyle$fontcolor,fg=plotstyle$frontcolor, col.axis=plotstyle$fontcolor, pch=plotstyle$plotpointminor)
			} else {
				textplot("no data", valign="top",col=plotstyle$fontcolor)
			} 
		}
		else {
			textplot("no data", valign="top",col=plotstyle$fontcolor)
		}
		mtext("Booted Moments", side=3, line=1 ,font=2 ,col=plotstyle$fontcolor, outer=TRUE)
		if(bootruns != -1) {
			bordertext2<-paste("Number of bootruns: ",bootruns, sep="")
			mtext(bordertext2, side=3, line=0, font=2, col=plotstyle$fontcolor, outer=TRUE)
		}		
		if(measurement_name != "") {
			bordertext3<-paste("Data: ",measurement_name, sep="")
			mtext(bordertext3, side=3, line=-1, font=2, col=plotstyle$fontcolor, outer=TRUE)
		}
	dev.off()
}
