\name{qat_save_boot_distribution_1d}
\alias{qat_save_boot_distribution_1d}
\title{Produce a savelist from a resultlist for a Boot Distribution Test}
\description{
This function takes the results, produced by qat\_analyse\_boot\_distribution\_1d and construct a savelist, which may be used to produce a netCDF output.}
\usage{
qat_save_boot_distribution_1d(resultlist_part, baseunit = "")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{resultlist_part}{A list with the results of the check}
  \item{baseunit}{The unit of the original measurement vector}
}
\details{
This function takes the resultslist and transfer the content to a newly organized list. Ths also consists of more information, which help to generate an output like a netCDF-file.}
\value{
Returning a savelist with the content of the resultlist.
}
\author{Andre Duesterhus}
\seealso{\code{\link{qat_call_save_boot_distribution}}, \code{\link{qat_run_workflow_save}}}
\examples{
	vec <- rnorm(1000)
	result <- list(result=qat_analyse_boot_distribution_1d(vec, 1000))
	savelist <- qat_save_boot_distribution_1d(result)	
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}