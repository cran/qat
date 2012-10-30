\name{qat_analyse_slide_distribution_2d}
\alias{qat_analyse_slide_distribution_2d}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Perform a slide distribution check}
\description{
The measurement vector will be scanned stepwise by a sliding window, and on every step some statistical parameters will be calculated.
}
\usage{
qat_analyse_slide_distribution_2d(measurement_vector, blocksize)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{measurement_vector}{The measurement vector (2d array), which should be tested}
  \item{blocksize}{Length of the sliding window}
}
\details{
The measurement vector will be scanned stepwise by a sliding window for each element of the second dimension, which got a length of the given parameter blocksize. At every step some statistical parameters will be calculated for the actual window. As a result a list will be given back, with these parameters, where every entry got the same dimension like the measurement vector, where the first dimension is reduced by the blocksize plus one.
}
\value{
It returns a list with the following entries:
  \item{first_moment}{First moment of the measurement vector}
  \item{second_moment}{Second moment of the measurement vector}
  \item{third_moment}{Third moment of the measurement vector}
  \item{fourth_moment}{Fourth moment of the measurement vector}
  \item{standard_deviation}{Standard deviation of the measurement vector}
  \item{skewness}{Skewness of the measurement vector}
  \item{kurtosis}{Kurtosis of the measurement vector}
  \item{median}{Median of the measurement vector}
  \item{p5_quantile}{5 percent quantile of the measurement vector}
  \item{p95_quantile}{95 percent quantile of the measurement vector}
  \item{p25_quantile}{25 percent quantile of the measurement vector}
  \item{p75_quantile}{75 percent quantile of the measurement vector}
  \item{blocksize}{Length of the used blocks}
}
\author{Andre Duesterhus}
\seealso{\code{\link{qat_analyse_slide_distribution_1d}}, \code{\link{qat_plot_slide_distribution_2d}}}
\examples{
vec <- array(rnorm(100),c(25,20))
result <- qat_analyse_slide_distribution_2d(vec, 5)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ts}
