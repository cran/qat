\name{qat_call_set_mean}
\alias{qat_call_set_mean}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Mean of values of a vector}
\description{
This function make a mean of sucessive values of a given vector.
}
\usage{
qat_call_set_mean(measurement_vector, workflowlist_part, element = -999, time = NULL, height = NULL, lat = NULL, lon = NULL, vec1 = NULL, vec2 = NULL, vec3 = NULL, vec4 = NULL, resultlist = list(), resultlistcounter = 1)
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
This function calls the described method, which are defined by the parameters in the workflowlist\_part. The possible called function is qat\_analyse\_set\_mean\_1d. As a result the function will give back a list, which include the corrected measurement vector.
}
\value{
Give back a list, which include the vector with the results of the block.
}
\author{Andre Duesterhus}
\seealso{\code{\link{qat_analyse_set_mean_1d}}}
\examples{
vec <- c(1,2,3,4,5,4,3,2,1)
workflowlist_part <- list(blocksize=3)
result <- qat_call_set_mean(vec, workflowlist_part)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ts}