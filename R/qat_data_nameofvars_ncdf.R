qat_data_nameofvars_ncdf <-
function(obj) {
## functionality: give back names of variables
## author: André Düsterhus
## date: 23.11.2009
## version: A0.1
## input: ncdf object
## output: list of names of variables
	namelist<-names(obj$var)
	return(namelist)
}

