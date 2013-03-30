\name{qat_save_result_ncdf}
\alias{qat_save_result_ncdf}
\title{Writing a savelist to a netCDF-file}
\description{
A savelist, which is constructed by the function qat\_run\_workflow\_save will be written to a given filename in netCDF format. Additional needed informations are the workflowlist, which constructed the savelist.
}
\usage{
qat_save_result_ncdf(measurement_vector, savelist, filename, workflowlist = NULL, time = NULL, height = NULL, lat = NULL, lon = NULL, vec1 = NULL, vec2 = NULL, vec3 = NULL, vec4 = NULL, store_mes_vec = TRUE, baseunit = "unitless", addunits = c("minutes", "metres", "degrees", "degrees", "unitless", "unitless", "unitless", "unitless"), directoryname = "", nan_value = -999, variable_name = "", transformationonvariable = "", authorname = "", original_filename = "", data_level = "", workflow_filename = "")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{measurement_vector}{The measurement vector, which was tested}
  \item{savelist}{The resulted savelist}
  \item{filename}{The name of the file, which should be written}
  \item{workflowlist}{The used workflowlist for the tests}
  \item{time}{A vector of time elements with the length of the measurement vector}
  \item{height}{A vector of height elements with the length of the measurement vector}
  \item{lat}{A vector of latitude elements with the length of the measurement vector}
  \item{lon}{A vector of longitude elements with the length of the measurement vector}
  \item{vec1}{An additional vector, which is named as vec1}
  \item{vec2}{An additional vector, which is named as vec2}
  \item{vec3}{An additional vector, which is named as vec3}
  \item{vec4}{An additional vector, which is named as vec4}
  \item{store_mes_vec}{A bolean variable if the measurement vector should also be stored}
  \item{baseunit}{Unit of the measurement vector}
  \item{addunits}{Vector of units for the other vectors}
  \item{directoryname}{Directory, where the resulting file should be stored}
  \item{nan_value}{Fill value for NaN in vectors}
  \item{variable_name}{Name of the original variable}
  \item{transformationonvariable}{Information on transformation of the original variable}
  \item{authorname}{Name of the author who performed the tests}
  \item{original_filename}{Filename, where the original data was stored}
  \item{data_level}{Data level of the original variable}
  \item{workflow_filename}{Filename of the workflow}
}
\details{
The savelist, which is a result of the function qat\_run\_workflow\_save, which transformed the resultlist of qat\_run\_workflow\_check to a here usable formate, delivers all necessary information to construct a netCDF-file. The workflowlist is needed, because further informations, like algorithms, descriptions and comments on results are simpler to edit in this list. This can be also saved by qat\_config\_write\_workflow to a XML-format. 
The netCDF-format used here is the QAD-convention. This allows to store the modifications of a tests and also the results into one file.
}
\value{
No return value.
}
\author{Andre Duesterhus}

\seealso{
 \code{\link{qat_run_workflow_save}}
}
\examples{
	library("qat")
	# define testvector
	testvector<-rnorm(500)
	# read in workflow from systemfiles
	filename_in <- system.file("extdata/workflowexample.xml", package="qat")
	workflowlist <- qat_config_read_workflow(filename_in)
	# define some additional vectors
	maxlim <- seq(3,1,length.out=500)
	minlim <- seq(-1,-3,length.out=500)
	uproc <- seq(1,3,length.out=500)
	downroc <- seq(3,1,length.out=500)
	# run the workflow on the testvector
	rlist <- qat_run_workflow_check(testvector,workflowlist,vec1=maxlim, vec2=minlim, vec3=uproc, vec4=downroc)
	# produce the savelist
	savelist <- qat_run_workflow_save(rlist)
	filename_out <- "myresults" 
	# write netCDF-file of the results in current directory
\dontrun{
	qat_save_result_ncdf(testvector, savelist=savelist, filename_out, workflowlist=workflowlist,vec1=maxlim, vec2=minlim, vec3=uproc, vec4=downroc)}}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}
