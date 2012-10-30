\name{qat_add_algorithm}
\alias{qat_add_algorithm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Algorithm of a check}
\description{
For each check in the workflow it is possible to add a algorithm of the test. This will be saved into the XML result file under agolgorithm. This function adds a new or replace an existing algorithm.
}
\usage{
qat_add_algorithm(workflowlist, listelem, algorithm_text)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{workflowlist}{A workflowlist like it will be created by qat\_config\_read\_workflow}
  \item{listelem}{Number of check, where the algorithm should be added.}
  \item{algorithm_text}{Text of the algorithm.}
}
\value{
Give back the edited workflowlist.
}
\author{Andre Duesterhus}
\seealso{\code{\link{qat_config_read_workflow}}}
\examples{
library("qat")
## read in workflow from systemfiles
# filename_in <- system.file("extdata/workflowexample.xml", package="qat")
# workflowlist <- qat_config_read_workflow(filename_in)
## add some more informations for the workflow
# workflowlist <- qat_add_algorithm(workflowlist, 1, "Algorithm information")
# filename_out <- "myworkflow_result.xml" 
## write edited workflow in current directory
# qat_config_write_workflow(workflowlist, output_filename=filename_out) 
}
\keyword{manip}