\name{qat_run_workflow_save}
\alias{qat_run_workflow_save}
\title{Performing a workflow of constructing saving elements by a given resultlist}
\description{
This function performs a workflow of constructing a savelist by a given resultlist.
}
\usage{
qat_run_workflow_save(resultlist, baseunit = "", time = NULL, height = NULL, lat = NULL, lon = NULL, vec1 = NULL, vec2 = NULL, vec3 = NULL, vec4 = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{  
  \item{resultlist}{Resultlist with results of checks}
  \item{baseunit}{Unit of the original measurement vector}
  \item{time}{A time vector of the measurment\_vector}
  \item{height}{A height vector of the measurment\_vector}
  \item{lat}{A latitude vector of the measurment\_vector}
  \item{lon}{A longitude vector of the measurment\_vector}
  \item{vec1}{A potential additional vector}
  \item{vec2}{A potential additional vector}
  \item{vec3}{A potential additional vector}
  \item{vec4}{A potential additional vector}
 }
\details{
This function performs a workflow of constructing a savelist by a given resultlist. This can be used to build netCDF-files by the function qat\_save\_result\_ncdf.
}
\value{
A savelist, with the results of the performed tests will be given back.
}
\author{Andre Duesterhus}
\seealso{\code{\link{qat_config_read_workflow}}, \code{\link{qat_run_workflow_check}}, \code{\link{qat_run_workflow_plot}}}
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
	qat_save_result_ncdf(testvector, savelist=savelist, filename_out, workflowlist=workflowlist ,vec1=maxlim, vec2=minlim, vec3=uproc, vec4=downroc)}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}
