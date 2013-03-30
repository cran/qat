\name{qat_save_histogram_test}
\alias{qat_save_histogram_test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Produce a savelist from a resultlist for a Histogram Test}
\description{
This function takes the results, produced by qat\_analyse\_histogram\_test\_xxx\_xd and construct a savelist, which may be used to produce a netCDF output.}
\usage{
qat_save_histogram_test(resultlist_part, baseunit = "")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{resultlist_part}{A list with the results of the check}
  \item{baseunit}{The unit of the original measurement vector}
}
\details{
This function takes the resultslist and transfer the content to a newly organized list. This also consists of more information, which help to generate an output like a netCDF-file.}
\value{
Returning a savelist with the content of the resultlist.
}
\author{Andre Duesterhus}
\seealso{\code{\link{qat_call_save_histogram_test}}, \code{\link{qat_run_workflow_save}}}
\examples{
vec <- c(rnorm(1000), rnorm(1000)+1)
workflowlist_part <- list(blocksize=50, numofbars=65, metric="emd")
resultlist <- qat_call_histogram_test(vec, workflowlist_part, element=1)	
savelist <- qat_save_histogram_test(resultlist[[2]])
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}