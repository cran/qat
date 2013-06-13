\name{qat_style_plot}
\alias{qat_style_plot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Produce a plotstylelist}
\description{
Loads a plotstyle, when a filename is given. When not a standard plotstyle will be given back
}
\usage{
qat_style_plot(filename = "")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filename}{Filename of a plotstyle-XML}
}
\details{
A plotstyle is a possibility to include a colorsheme in every plot, which is produced by the qat-package. With given filename a certain plotstyle will be loaded. Without a standard sheme will be used.
}
\value{
A list with the inforamtion of the colorsheme.
}
\author{Andre Duesterhus}
\seealso{\code{\link{qat_run_workflow_plot}}}
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
# load plotstyle
filename_ps <- system.file("extdata/plotstyle1.xml", package="qat")
ps<-qat_style_plot(filename_ps)
# run the workflow on the testvector
rlist <- qat_run_workflow_check(testvector,workflowlist,vec1=maxlim, vec2=minlim, 
vec3=uproc, vec4=downroc)
# produce some plots of the result in teh current directory with new plotstyle
qat_run_workflow_plot(rlist, measurement_name="Test", basename="test", plotstyle=ps)
# add some more informations for the workflow
workflowlist <- qat_add_all_descriptions(workflowlist) 
workflowlist <- qat_add_all_algorithms(workflowlist)
workflowlist <- qat_add_comment(workflowlist, 1, "No problems")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{iplot}
\keyword{IO}