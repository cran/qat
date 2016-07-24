qat_data_read_ncdf <-
function(filename) {
## functionality: reads netcdf data
## author: André Düsterhus
## date: 30.10.2012
## version: A0.2
## input: filename of an existing netcdf-file
## output: ncdf object
#	library("ncdf4")
	ncdfobj<-nc_open(filename)
	return(ncdfobj)
}
