\name{qat_add_comment}
\alias{qat_add_comment}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Comment on result}
\description{
For each check in the workflow it is possible to add a comment on the result of the test. This will be saved into the XML result file under the tag result/comment\_on\_result. This function adds a new or replace an existing comment.
}
\usage{
qat_add_comment(workflowlist, listelem, comment_text)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{workflowlist}{A workflowlist like it will be created by qat\_config\_read\_workflow}
  \item{listelem}{Number of check, which should be commented.}
  \item{comment_text}{Text of the comment}
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
workflowlist <- qat_add_comment(workflowlist, 1, "No problems")
filename_out <- "myworkflow_result.xml" 
# write edited workflow in current directory
qat_config_write_workflow(workflowlist, output_filename=filename_out) 
}
\keyword{manip}