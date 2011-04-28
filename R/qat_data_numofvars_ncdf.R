qat_data_numofvars_ncdf <-
function(obj) {
## functionality: give back number of variables
## author: André Düsterhus
## date: 23.11.2009
## version: A0.1
## input: ncdf object
## output: number of variables
	lenobj<-length(obj$var)
	return(lenobj)
}

