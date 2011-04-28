\name{qat_config_write_workflow}
\alias{qat_config_write_workflow}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Write a result}
\description{
A workflowlist will be stored into a XML-file.
}
\usage{
qat_config_write_workflow(workflowlist, name = "", description = "", author = "", date = "", sample_time_start = "", sample_time_stop = "", sample_place = "", config_filename = "", output_filename = "")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{workflowlist}{A workflowlist, which may be loaded by qat\_config\_read\_workflow and used for tests.}
  \item{name}{Name of the tests, which were performed with this workflowlist}
  \item{description}{Description of the workflowlist}
  \item{author}{Author who used the workflowlist for a test.}
  \item{date}{Date of the test.}
  \item{sample_time_start}{Start time of the sample, which was tested}
  \item{sample_time_stop}{End time of the sample, which was tested}
  \item{sample_place}{Location of the sample, which was tested}
  \item{config_filename}{A filename of the configuration file, which was read in at qat\_config\_read\_workflow.}
  \item{output_filename}{Filename, where the result should be stored.}
}
\details{
The workflow will be stored at the location of output\_filename. As additional information in the header of this file, the other arguments will be used.
}
\value{
The information, which was stored, will be given back.
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
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{IO}