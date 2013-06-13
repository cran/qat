\name{qat-package}
\alias{qat-package}
\alias{qat}
\docType{package}
\title{
Quality Assurance Toolkit
}
\description{
This package helps to provide a quality assurance on data.
}
\details{
\tabular{ll}{
Package: \tab qat\cr
Type: \tab Package\cr
Version: \tab 0.72\cr
Date: \tab 2013-06-13\cr
License: \tab GPL-2\cr
}

}
\author{
Andre Duesterhus \cr
Maintainer: Andre Duesterhus <andue@uni-bonn.de>
}
\keyword{ package }
\examples{
library("qat")
# define testvector
testvector<-rnorm(200)
# read in workflow from systemfiles
filename_in <- system.file("extdata/workflowexample.xml", package="qat")
workflowlist <- qat_config_read_workflow(filename_in)
# define some additional vectors
maxlim <- seq(3,1,length.out=200)
minlim <- seq(-1,-3,length.out=200)
uproc <- seq(1,3,length.out=200)
downroc <- seq(3,1,length.out=200)
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
