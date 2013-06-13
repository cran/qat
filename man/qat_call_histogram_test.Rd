\name{qat_call_histogram_test}
\alias{qat_call_histogram_test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Perform a LIM Rule Check}
\description{
This function calls the described tests, which are defined by the parameters in the workflowlist\_part. The possible called functions are qat\_analyse\_histogram\_test\_xxx\_xd.
}
\usage{
qat_call_histogram_test(measurement_vector, workflowlist_part, element = -999, 
time = NULL, height = NULL, lat = NULL, lon = NULL, vec1 = NULL, vec2 = NULL, 
vec3 = NULL, vec4 = NULL, resultlist = list(), resultlistcounter = 1)
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
This function calls the described tests, which are defined by the parameters in the workflowlist\_part. The possible called functions are qat\_analyse\_lim\_rule\_dynamic\_1d, qat\_analyse\_lim\_rule\_static\_1d and qat\_analyse\_lim\_rule\_sigma\_1d. As a result the resultlist will get additional entries, which are defined by the tests, which may called by this function.
}
\value{
The given resultlist will be returned, with included results of the functions which may called in this function.
}
\author{Andre Duesterhus}
\seealso{\code{\link{qat_analyse_histogram_test_kld_1d}}, \code{\link{qat_analyse_histogram_test_jsd_1d}}, \code{\link{qat_analyse_histogram_test_rms_1d}}, \code{\link{qat_analyse_histogram_test_ms_1d}}, \code{\link{qat_analyse_histogram_test_emd_1d}}, \code{\link{qat_analyse_histogram_test_kld_2d}}, \code{\link{qat_analyse_histogram_test_jsd_2d}}, \code{\link{qat_analyse_histogram_test_rms_2d}}, \code{\link{qat_analyse_histogram_test_ms_2d}}, \code{\link{qat_analyse_histogram_test_emd_2d}}
}
\examples{
vec <- c(rnorm(1000), rnorm(1000)+1)
workflowlist_part <- list(blocksize=50, numofbars=65, metric="emd")
resultlist <- qat_call_histogram_test(vec, workflowlist_part, element=1)	
savelist <- qat_call_save_histogram_test(resultlist[[2]])
}
\keyword{utilities}