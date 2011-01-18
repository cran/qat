qat_data_read_ncdf <-
function(filename) {
## functionality: reads netcdf data
## author: André Düsterhus
## date: 23.11.2009
## version: A0.1
## input: filename of an existing netcdf-file
## output: ncdf object
	library("ncdf")
	ncdfobj<-open.ncdf(filename)
	return(ncdfobj)
}
