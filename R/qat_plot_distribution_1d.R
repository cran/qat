qat_plot_distribution_1d <-
function(resultlist_hist, filename, resultlist_stat, numofbars=-1, measurement_name="", directoryname="", plotstyle=NULL) {
library(gplots)
## functionality: plot the propability distribution and statistical parameters by using a resultlist of qat_analyse_distribution_1d
## author: André Düsterhus
## date: 23.02.2010
## version: A0.1
## input: resultlist from qat_analyse_distribution, directoryname, filename, plotstylelist
## output: plot
	if (is.null(plotstyle)) {
		# if no plotstyle available, use standard plotstyle
		plotstyle<-qat_style_plot()
	}
	# set up savepath of the plot
	path <- paste(directoryname,filename,".png", sep="")
	# forming of headline
	if(measurement_name != "") {
		bordertext3<-paste("Histogram of ",measurement_name, sep="")
	} else {
		bordertext3<-"Histogram of Measurement"
	}
#	print(resultlist_hist)
	if (!is.nan(resultlist_hist) || !is.nan(resultlist_stat)) {
		png(filename=path, width=800, height=600, pointsize=12, bg=plotstyle$basecolor)
			par(font.lab=2)
			layout(matrix(c(1,2),nrow=1), width=c(2,1))
			if ((sum(is.nan(resultlist_hist$counts)) != length(resultlist_hist$counts))&& (sum(resultlist_hist$density[!is.nan(resultlist_hist$density)]) != Inf ) ) {
				plot(resultlist_hist, col=plotstyle$plotcolormain,main=bordertext3, xlab="Data", ylab="Frequency",font=2, col.lab=plotstyle$fontcolor, col.main=plotstyle$fontcolor, col.sub=plotstyle$fontcolor,fg=plotstyle$frontcolor, col.axis=plotstyle$fontcolor, border=plotstyle$frontcolor)
			}
			par(font=2)
			# produce statistical information text
			text <- "Statistical Moments\n"
			if ("first_moment" %in% names(resultlist_stat)) {
				text<-paste(text,"1st moment: ", round(resultlist_stat$first_moment, digits = 3),"\n", sep="")
			}
			if ("second_moment" %in% names(resultlist_stat)) {
				text<-paste(text,"2nd moment: ", round(resultlist_stat$second_moment, digits = 3),"\n", sep="")
			}
			if ("third_moment" %in% names(resultlist_stat)) {
				text<-paste(text,"3rd moment: ", round(resultlist_stat$third_moment, digits = 3),"\n", sep="")
			}
			if ("fourth_moment" %in% names(resultlist_stat)) {
				text<-paste(text,"4th moment: ", round(resultlist_stat$fourth_moment, digits = 3),"\n", sep="")
			}
			if ("standard_deviation" %in% names(resultlist_stat)) {
				text<-paste(text,"Std. Dev.:  ",round(resultlist_stat$standard_deviation, digits = 3),"\n", sep="")
			}
			if ("skewness" %in% names(resultlist_stat)) {
				text<-paste(text,"Skewness:   ",round(resultlist_stat$skewness, digits = 3),"\n", sep="")
			}
			if ("kurtosis" %in% names(resultlist_stat)) {
				text<-paste(text,"Kurtosis:   ",round(resultlist_stat$kurtosis, digits = 3),"\n", sep="")
			}
			if ("p5_quantile" %in% names(resultlist_stat)) {
				text<-paste(text,"5p quant.:  ",round(resultlist_stat$p5_quantile, digits = 3),"\n", sep="")
			}
			if ("p25_quantile" %in% names(resultlist_stat)) {
				text<-paste(text,"25p quant.: ",round(resultlist_stat$p25_quantile, digits = 3),"\n", sep="")
			}		
			if ("median" %in% names(resultlist_stat)) {
				text<-paste(text,"Median:     ",round(resultlist_stat$median, digits = 3),"\n", sep="")
			}	
			if ("p25_quantile" %in% names(resultlist_stat)) {
				text<-paste(text,"75p quant.: ",round(resultlist_stat$p75_quantile, digits = 3),"\n", sep="")
			}	
			if ("p95_quantile" %in% names(resultlist_stat)) {
				text<-paste(text,"95p quant.: ",round(resultlist_stat$p95_quantile, digits = 3),"\n", sep="")
			}		
			if (numofbars != -1) {
				text<-paste(text,"\nNumOfBars:  ",numofbars,"\n", sep="")
			}	
			textplot(text,valign="top",col=plotstyle$fontcolor)
		dev.off()
	}
}

