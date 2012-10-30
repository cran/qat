\name{qat_analyse_trimmed_distribution_2d}
\alias{qat_analyse_trimmed_distribution_2d}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Perform a trimmed distribution check}
\description{
The measurement vector (2d array) will be handled separately for every element in the direction of the second dimension. Each vector will be trimmed stepwise at each side and at every step some statistical parameters will be calculated.
}
\usage{
qat_analyse_trimmed_distribution_2d(measurement_vector)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{measurement_vector}{The measurement vector, which should be tested}
}
\details{
The measurement vector will be trimmed, in direction of the first dimension, at each side stepwise, with a step of 1 percent. At each step some statistical parameters will be calculated. As a result a list will be given back, with these parameters, where every entry got the first dimension of 50 and as the second the second dimension of the measurement vector.
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
}
\author{Andre Duesterhus}
\seealso{\code{\link{qat_analyse_trimmed_distribution_1d}}, \code{\link{qat_plot_trimmed_distribution_2d}}}
\examples{
vec <- array(rnorm(100),c(25,20))
result <- qat_analyse_trimmed_distribution_2d(vec)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ts}
