\name{qat_analyse_distribution_2d}
\alias{qat_analyse_distribution_2d}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Perform a distribution check}
\description{
This check makes a histogram and gives back some statistical parameters of the given measurement vector.
}
\usage{
qat_analyse_distribution_2d(measurement_vector, numofbars)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{measurement_vector}{The measurement vector (2d array), which should be tested}
  \item{numofbars}{Numbers of bars of the histogram plot}
}
\details{
From a given measurement vector (2d array) a histogram will be performed. The number of bars of this will be given by the parameter numofbars. Additionally some statistical parameters, like the first moments and some quantiles will be calculated.
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
  \item{numofbars}{Number of bars of the histogram}
  \item{...}{Elements of the histogram}
}
\author{Andre Duesterhus}
\seealso{\code{\link{qat_analyse_distribution_1d}}, \code{\link{qat_plot_distribution_1d}}}
\examples{
vec <- array(rnorm(500),c(25,20))
result <- qat_analyse_distribution_2d(vec, 10)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ts}
