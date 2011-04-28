\name{qat_add_all_descriptions}
\alias{qat_add_all_descriptions}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Adds all descriptions to a workflow}
\description{
For each check in the workflow it is possible to add a description of the test. This will be saved into the XML result file under the description. This function adds for each test the known description-information.
}
\usage{
qat_add_all_descriptions(workflowlist)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{workflowlist}{A workflowlist like it will be created by qat\_config\_read\_workflow}
}
\details{
This function use the informatio, which is stored in the system file qat\_basetools.xml. 
}
\value{
Give back the edited workflowlist.
}
\author{Andre Duesterhus}
\seealso{\code{\link{qat_config_read_workflow}}}
\examples{
library("qat")
# read in workflow from systemfiles
filename_in <- system.file("extdata/workflowexample.xml", package="qat")
workflowlist <- qat_config_read_workflow(filename_in)
# add some more informations for the workflow
workflowlist <- qat_add_all_descriptions(workflowlist) 
workflowlist <- qat_add_all_algorithms(workflowlist)

filename_out <- "myworkflow_result.xml" 
# write edited workflow in current directory
qat_config_write_workflow(workflowlist, output_filename=filename_out) 
}
\keyword{manip}