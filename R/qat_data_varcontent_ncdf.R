qat_data_varcontent_ncdf <-
function(obj,numofvar) {
## functionality: give back content of a variable
## author: André Düsterhus
## date: 23.11.2009
## version: A0.1
## input: ncdf object, number of variable
## output: content of variable
	var<-obj$var[[numofvar]]
	varcontent<-get.var.ncdf(obj,var)
	return(varcontent)
}
