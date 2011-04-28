\name{qat_run_workflow_check}
\alias{qat_run_workflow_check}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Perform a workflow of checks}
\description{
This function performs a workflow of checks by a given workflowlist on a given vector.
}
\usage{
qat_run_workflow_check(measurement_vector, workflowlist, time = NULL, height = NULL, lat = NULL, lon = NULL, vec1 = NULL, vec2 = NULL, vec3 = NULL, vec4 = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{measurement_vector}{The measurement vector, which should be tested}
  \item{workflowlist}{The workflowlist, which should be performed.}
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
This function performs a workflow of checks by a given workflowlist on a given measurement vector. Additional vectors can be used in the tests.
}
\value{
A resultlist, with the results of the performed tests will be given back.
}
\author{Andre Duesterhus}
\seealso{\code{\link{qat_config_read_workflow}}, \code{\link{qat_run_workflow_plot}}}
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
# produce some plots of the result in teh current directory
qat_run_workflow_plot(rlist, measurement_name="Test", basename="test")
# add some more informations for the workflow
workflowlist <- qat_add_all_descriptions(workflowlist) 
workflowlist <- qat_add_all_algorithms(workflowlist)
workflowlist <- qat_add_comment(workflowlist, 1, "No problems")

filename_out <- "myworkflow_result.xml" 
# write edited workflow in current directory
qat_config_write_workflow(workflowlist, output_filename=filename_out) 
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}