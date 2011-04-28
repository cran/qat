qat_plot_lim_rule_sigma_1d <-
function(flagvector, filename, measurement_vector=NULL, sigma_factor=NULL, meanofvector=NaN, sdofvector=NULL, measurement_name="",directoryname="", plotstyle=NULL) {
## functionality: plot lim-static-rule
## author: André Düsterhus
## date: 02.03.2010
## version: A0.1
## input: flagvector from qat_analyse_lim_rule_sigma, measurement_vector, sigma factor, mean of vector, standard deviation of vector, directoryname, filename, plotstylelist
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
			plot(flagvector, col=plotstyle$plotcolorminor, xlab="", ylab="", type="p", col.lab=plotstyle$fontcolor, col.main=plotstyle$fontcolor, col.sub=plotstyle$fontcolor, fg=plotstyle$frontcolor, col.axis=plotstyle$fontcolor, font.axis=2)
			title(main=list("LIM-Rule static", col=plotstyle$fontcolor), outer=TRUE)
		dev.off()
	} else {
		png(filename=path,width=800,height=600, pointsize=12, bg=plotstyle$basecolor)
			par(font.lab=2,oma=c(0,0,2,0))
			plot(1:length(measurement_vector), measurement_vector, col=plotstyle$plotcolormain, xlab="", ylab="", type="n", col.lab=plotstyle$fontcolor, col.main=plotstyle$fontcolor, col.sub=plotstyle$fontcolor,fg=plotstyle$frontcolor, col.axis=plotstyle$fontcolor, pch=plotstyle$plotpointmain, font.axis=2,xlim=c(1, length(flagvector)), ylim=c(min(measurement_vector,na.rm =TRUE),max(measurement_vector,na.rm =TRUE)))
			if(!is.na(sigma_factor)&&!is.na(meanofvector)&&!is.na(sdofvector)) {
				rect(0,meanofvector-sigma_factor*sdofvector,length(measurement_vector),meanofvector+sigma_factor*sdofvector, lwd=0, border = NA,col =plotstyle$plotcolorbackground)
				abline(h=meanofvector-sigma_factor*sdofvector, col=plotstyle$frontcolor)
				abline(h=meanofvector+sigma_factor*sdofvector, col=plotstyle$frontcolor)
				bordertext2 <- paste("Minimum errors: ",length(which(flagvector==-1)), " Maximum errors: ",length(which(flagvector==1)))
			} else {
				bordertext2<-""
			}
			points(which(flagvector != 0), measurement_vector[which(flagvector != 0)], col=plotstyle$plotcolormain, xlab="", ylab="", type="p", col.lab=plotstyle$fontcolor, col.main=plotstyle$fontcolor, col.sub=plotstyle$fontcolor,fg=plotstyle$frontcolor, col.axis=plotstyle$fontcolor, pch=plotstyle$plotpointmain)
			points(which(flagvector == 0), measurement_vector[which(flagvector == 0)], col=plotstyle$plotcolorminor, xlab="", ylab="", type="p", col.lab=plotstyle$fontcolor, col.main=plotstyle$fontcolor, col.sub=plotstyle$fontcolor,fg=plotstyle$frontcolor, col.axis=plotstyle$fontcolor, pch=plotstyle$plotpointminor)
			if(!is.na(sigma_factor)) {
				bordertext<- paste("Sigma factor: ", sigma_factor, " ")
			} else {
				bordertext <- ""
			}
			if(!is.na(meanofvector)) {
				bordertext <- paste(bordertext,"Mean of vector: ", meanofvector, " ")
			} else {
				bordertext<- paste(bordertext,"")
			}
			if(!is.na(sdofvector) != 0) {
				bordertext <- paste(bordertext,"SD of vector: ", sdofvector)
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
			title(main=list("LIM-Rule sigma", col=plotstyle$fontcolor), outer=TRUE)
		dev.off()
	}
}

