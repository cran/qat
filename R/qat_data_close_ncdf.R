qat_data_close_ncdf <-
function(obj) {
## functionality: close an open ncdf file
## author: André Düsterhus
## date: 23.05.2012
## version: A0.2
## input: filename of an existing netcdf-file
## output: ncdf object
	library("ncdf")
	close.ncdf(obj)
}
