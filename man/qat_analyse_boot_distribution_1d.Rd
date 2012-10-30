\name{qat_analyse_boot_distribution_1d}
\alias{qat_analyse_boot_distribution_1d}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Perform a bootstrapped distribution check}
\description{
The measurement vector will be bootstrapped and statistical parameters will be determined.
}
\usage{
qat_analyse_boot_distribution_1d(measurement_vector, bootruns)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{measurement_vector}{The measurement vector, which should be tested}
  \item{bootruns}{Number of bootstrap runs, which should be performed}
}
\details{
The measurement vector will be bottstrapped with the number of runs, which is given by the parameter bootruns. From each runs, some statistical parameters will be calculated and given back in the resultlist.
}
\value{
It returns a list with the following entries:
  \item{first_moment}{First moments of the bootstrapped measurement vector}
  \item{second_moment}{Second moments of the bootstrapped measurement vector}
  \item{third_moment}{Third moments of the bootstrapped measurement vector}
  \item{fourth_moment}{Fourth moments of the bootstrapped measurement vector}
  \item{standard_deviation}{Standard deviations of the bootstrapped measurement vector}
  \item{skewness}{Skewness of the bootstrapped measurement vector}
  \item{kurtosis}{Kurtosis of the bootstrapped measurement vector}
  \item{median}{Medians of the bootstrapped measurement vector}
  \item{p5_quantile}{5 percent quantiles of the bootstrapped measurement vector}
  \item{p95_quantile}{95 percent quantiles of the bootstrapped measurement vector}
  \item{p25_quantile}{25 percent quantiles of the bootstrapped measurement vector}
  \item{p75_quantile}{75 percent quantiles of the bootstrapped measurement vector}
}
\author{Andre Duesterhus}
\seealso{\code{\link{qat_plot_boot_distribution_1d}}}
\examples{
vec <- rnorm(1000)
result <- qat_analyse_boot_distribution_1d(vec, 1000)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ts}