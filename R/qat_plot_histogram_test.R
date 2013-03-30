qat_plot_histogram_test <-
function(resultfield, filename, blocksize=-1, numofbars=-1, factorofbar=-1, metric=NULL, runs=NULL, measurement_name="", directoryname="", plotstyle=NULL) {
## functionality: plot result field of a histogram test
## author: André Düsterhus
## date: 22.03.2013
## version: A0.1
## input: resultlist from qat_analyse_histogram_test_xxx, directoryname, filename, plotstylelist
## output: plot
	if (is.null(plotstyle)) {
		# if no plotstyle available, use standard plotstyle
		plotstyle<-qat_style_plot()
	}
	library("gplots")
	library("fields")
	path <- paste(directoryname,filename,".png", sep="")
	png(filename=path,width=800,height=600, pointsize=12, bg=plotstyle$basecolor)
	par(font.lab=2, mar=c(5.1,5.1,5.1,8), oma=c(0,0,2,0), font=2)
	if (length(dim(resultfield))==2) {
		image(1:dim(resultfield)[1], 1:dim(resultfield)[2], resultfield[ , dim(resultfield)[2]:1], col=tim.colors(64), xlab="", ylab="", col.lab=plotstyle$fontcolor, col.main=plotstyle$fontcolor, col.sub=plotstyle$fontcolor,fg=plotstyle$frontcolor, col.axis=plotstyle$fontcolor, font.axis=2)
		if(!is.null(metric)) {
			mtext(paste("Histogram test using the ",toupper(metric), sep=""), side=3, line=1 , font=2 , col=plotstyle$fontcolor, outer=TRUE)
		} else {
			mtext(paste("Histogram test"), side=3, line=1 ,font=2 ,col=plotstyle$fontcolor, outer=TRUE)			
		}
		bordertext2 <- ""
		if(blocksize != -1) {
			bordertext2<- paste("Blocksize: ", blocksize, "; ", sep="")
		}
		if(numofbars != -1) {
			bordertext2<- paste(bordertext2, "Number of Bars: ", numofbars, "; ", sep="")
		}
		if(factorofbar != -1) {
			bordertext2<- paste(bordertext2, "Factor of Bars: ", factorofbar, sep="")
		}
		if (bordertext2 != "") {
			mtext(bordertext2, side=3, line=0, font=2, col=plotstyle$fontcolor, outer=TRUE)	
		}
		if(measurement_name != "") {
			bordertext3<-paste("Data: ",measurement_name, sep="")
			mtext(bordertext3, side=3, line=-1, font=2, col=plotstyle$fontcolor, outer=TRUE)
		}	
		image.plot(1:dim(resultfield)[1], 1:dim(resultfield)[2], resultfield, col=tim.colors(64), xlab="", ylab="", col.lab=plotstyle$fontcolor, col.main=plotstyle$fontcolor, col.sub=plotstyle$fontcolor,fg=plotstyle$frontcolor, col.axis=plotstyle$fontcolor, font.axis=2, legend.only=T)
	} else {
		textplot("no data", valign="top",col=plotstyle$fontcolor)
	}
	dev.off()
}
