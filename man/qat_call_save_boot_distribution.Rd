\name{qat_call_save_boot_distribution}
\alias{qat_call_save_boot_distribution}
\title{Produce a savelist-entry for a Boot Distribution Test}
\description{
This function calls qat\_save\_boot\_distribution\_1d. As a result a part of a savelist is constructed, which can be used to construct a netCDF file.}
\usage{
qat_call_save_boot_distribution(resultlist_part, element = -999, dim_mv=1, time = NULL, height = NULL, lat = NULL, lon = NULL, vec1 = NULL, vec2 = NULL, vec3 = NULL, vec4 = NULL, baseunit = NULL, savelist = list(), savelistcounter = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{resultlist_part}{A list with the results of the check}
  \item{element}{Element-identifier for the result, which will be given back in the savelist}
  \item{dim_mv}{Dimension of the measurement vector.}
  \item{time}{A vector of time elements with the length of the measurement vector}
  \item{height}{A vector of height elements with the length of the measurement vector}
  \item{lat}{A vector of latitude elements with the length of the measurement vector}
  \item{lon}{A vector of longitude elements with the length of the measurement vector}
  \item{vec1}{An additional vector, which is named as vec1}
  \item{vec2}{An additional vector, which is named as vec2}
  \item{vec3}{An additional vector, which is named as vec3}
  \item{vec4}{An additional vector, which is named as vec4}
  \item{baseunit}{The unit of the original measurement vector}
  \item{savelist}{A list with save elements}
  \item{savelistcounter}{Numbers of elements of the savelist}
}
\details{
This function calls the described saving-function, which transform the resultlist elements to a savinglist element. The possible called function is qat\_save\_boot\_distribution\_1d. As a result the given savelist will get an additional entry.
}
\value{
The given savelist will be returned, with included results of the functions which may be called in this function.
}

\author{Andre Duesterhus}
\seealso{\code{\link{qat_save_boot_distribution_1d}}, \code{\link{qat_run_workflow_save}}}

\examples{
	vec <- rnorm(1000)
	workflowlist_part <- list(bootruns=1000)
	resultlist <- qat_call_boot_distribution(vec, workflowlist_part, element=1)	
	savelist <- qat_call_save_boot_distribution(resultlist[[2]])}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}