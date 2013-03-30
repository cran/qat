qat_save_histogram_test <-
function(resultlist_part, baseunit="") {
## functionality: save distribution
## author: André Düsterhus
## date: 23.03.2013
## version: A0.1
## input: resultlist part from qat_analyse_histogram_test_xxx, optional: baseunit
## output: savelist
	method <- "histogram_test"
	meanings <- list(resultmatrix = "Result matrix")
	longname <- list(resultmatrix = "Result matrix of histogram test")
	fillvalue <- -999
	unit <- list(resultmatrix="unitless")
	blocknum <- dim(resultlist_part$result$field)[1]
	dimension <- list(resultmatrix=list(runs=resultlist_part$result$runs, runs=resultlist_part$result$runs))
	parameter <- list(blocksize=resultlist_part$result$blocksize, numofbars=resultlist_part$result$numofbars, metric=resultlist_part$result$metric, runs=resultlist_part$result$runs)
	picnames <- list(firstpic = paste("histogramtest_", resultlist_part$result$metric,sep=""))
	content <-  list(resultmatrix=resultlist_part$result$field)
	savelist <- list(method = method, meanings = meanings, longname = longname, fillvalue = fillvalue, unit = unit, dimension = dimension, parameter = parameter, picnames=picnames, content = content)
	return(savelist)	
}
