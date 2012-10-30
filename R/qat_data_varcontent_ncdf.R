qat_data_varcontent_ncdf <-
function(obj,numofvar) {
## functionality: give back content of a variable
## author: André Düsterhus
## date: 30.10.2012
## version: A0.2
## input: ncdf object, number of variable
## output: content of variable
	library("ncdf4")
	var<-obj$var[[numofvar]]
	varcontent<-ncvar_get(obj,var)
	return(varcontent)
}
