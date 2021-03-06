\name{qat_save_lim_rule_sigma_2d}
\alias{qat_save_lim_rule_sigma_2d}
\title{Produce a savelist from a resultlist for a LIM Rule Sigma Test}
\description{
This function takes the results, produced by qat\_analyse\_lim\_rule\_sigma\_2d and construct a savelist, which may be used to produce a netCDF output.}
\usage{
qat_save_lim_rule_sigma_2d(resultlist_part, baseunit = "")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{resultlist_part}{A list with the results of the check}
  \item{baseunit}{The unit of the original measurement vector}
}
\details{
This function takes the resultlist and transfer the content to a newly organized list. This also consists of more information, which help to generate an output like a netCDF-file.}
\value{
Returning a savelist with the content of the resultlist.
}
\author{Andre Duesterhus}
\seealso{\code{\link{qat_call_save_lim_rule}}, \code{\link{qat_run_workflow_save}}}
\examples{
	vec <- array(rnorm(1000), c(10, 100))
	result <- list(result=qat_analyse_lim_rule_sigma_2d(vec, 2))
	savelist <- qat_save_lim_rule_sigma_2d(result)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}