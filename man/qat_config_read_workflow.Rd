\name{qat_config_read_workflow}
\alias{qat_config_read_workflow}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Read an XML workflow}
\description{
This functions read a XML-workflow-file.
}
\usage{
qat_config_read_workflow(filename)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filename}{Path and filename of the xml-file, in which the workflow is defined}
}
\details{
This functions read a file, which got a XML-workflow in it. This will be transformed to a workflowlist, which may be processed by qat\_run\_workflow\_check.
}
\value{
A workflowlist, which consists of the tests and its parameters, which should be performed.
}
\author{Andre Duesterhus}
\seealso{\code{\link{qat_run_workflow_check}}}
\examples{
library("qat")
# read in workflow from systemfiles
filename_in <- system.file("extdata/workflowexample.xml", package="qat")
workflowlist <- qat_config_read_workflow(filename_in)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{IO}