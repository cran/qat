\name{qat_call_noc_rule}
\alias{qat_call_noc_rule}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Perform a NOC Rule Check}
\description{
This function calls the described tests, which are defined by the parameters in the workflowlist\_part. The possible called functions are qat\_analyse\_noc\_rule\_1d.}
\usage{
qat_call_noc_rule(measurement_vector, workflowlist_part, element = -999, time = NULL, height = NULL, lat = NULL, lon = NULL, vec1 = NULL, vec2 = NULL, vec3 = NULL, vec4 = NULL, resultlist = list(), resultlistcounter = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{measurement_vector}{The measurement vector, which should be tested}
  \item{workflowlist_part}{A list with the parameters of the check}
  \item{element}{Element-identifier for the result, which will be given back in the resultlist}
  \item{time}{A vector of time elements with the length of the measurement vector}
  \item{height}{A vector of height elements with the length of the measurement vector}
  \item{lat}{A vector of latitude elements with the length of the measurement vector}
  \item{lon}{A vector of longitude elements with the length of the measurement vector}
  \item{vec1}{An additional vector, which is named as vec1}
  \item{vec2}{An additional vector, which is named as vec2}
  \item{vec3}{An additional vector, which is named as vec3}
  \item{vec4}{An additional vector, which is named as vec4}
  \item{resultlist}{A list with results of tests}
  \item{resultlistcounter}{Number of elements of the resultlist}
}
\details{
This function calls the described tests, which are defined by the parameters in the workflowlist\_part. The possible called functions are qat\_analyse\_noc\_rule\_1d. As a result the resultlist will get additional entries, which are defined by the tests, which may called by this function.
}
\value{
The given resultlist will be returned, with included results of the functions which may called in this function.
}
\author{Andre Duesterhus}
\seealso{\code{\link{qat_plot_noc_rule_1d}}}
\examples{
vec <- c(1,2,3,4,4,4,5,5,4,3,NaN,3,2,1)
workflowlist_part <- list(max_return_elements=1)
result <- qat_call_noc_rule(vec, workflowlist_part)
}
\keyword{utilities}