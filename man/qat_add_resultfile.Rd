\name{qat_add_resultfile}
\alias{qat_add_resultfile}
\title{Resultfile of a check}
\description{
For each check in the workflow it is possible to add one or more result files of the test. This will be saved into the XML result file under result\_file. This function adds a new resultfile.
}
\usage{
qat_add_resultfile(workflowlist, listelem, resultfile_text)
}
\arguments{
  \item{workflowlist}{A workflowlist like it will be created by qat\_config\_read\_workflow.}
  \item{listelem}{Number of check, where the resultfile should be added.}
  \item{resultfile_text}{Text of the resultfile.}
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
workflowlist <- qat_add_resultfile(workflowlist, 1, "filename.png")
filename_out <- "myworkflow_result.xml" 
# write edited workflow in current directory
qat_config_write_workflow(workflowlist, output_filename=filename_out) 
}
\keyword{manip}