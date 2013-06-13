\name{qat_run_workflow_plot}
\alias{qat_run_workflow_plot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Produce plots of a workflow}
\description{
This function produces plots of the results, which were produced by a workflow.
}
\usage{
qat_run_workflow_plot(resultlist, measurement_name = "", directoryname = "", 
basename = "", plotstyle = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{resultlist}{The results, which are produced by qat\_run\_workflow\_check}
  \item{measurement_name}{The measurement vector, which is used at the tests}
  \item{directoryname}{Directory, where the resulting plots should be stored}
  \item{basename}{Basic name of the filename}
  \item{plotstyle}{A list with a qat color scheme.}
}
\details{
The resultlist contains the parameters and results of the tests. From this the plots will be constructed and stored in the given directory. As filename the basename is used, with further extensions to indicate the tests. When no plotstyle is defined the standard-colorscheme will be used. 
}
\value{
No return value.
}
\author{Andre Duesterhus}
\seealso{\code{\link{qat_run_workflow_check}}}
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
rlist <- qat_run_workflow_check(testvector,workflowlist,vec1=maxlim, vec2=minlim, 
vec3=uproc, vec4=downroc)
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