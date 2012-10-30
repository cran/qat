\name{qat_analyse_lim_rule_sigma_2d}
\alias{qat_analyse_lim_rule_sigma_2d}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Perform a sigma lim-rule-check}
\description{
This check tests data on whether it exceeds a threshold formed by multiple standard derviations away from the mean.
}
\usage{
qat_analyse_lim_rule_sigma_2d(measurement_vector, sigma_factor)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{measurement_vector}{The measurement vector (2d array), which should be tested}
  \item{sigma_factor}{Multiplier of standard derivation, which determin the maximum allowed deviation from the mean}
}
\details{
First the mean and the standard derivation of the measurement vector will be calculated. After this the limits will be determined by 
\deqn{lim_{\pm} = \mu \pm f \sigma,} where f is the given sigma factor.
}
\value{
It returns a list with the following entries:
  \item{flagvector}{A vector of length of measurement vector. For every element of the measurement vector the flagvector (2d array) contains a -1, if its exceeding its dedicated minimum vector element, a 1, if its exceeding its dedicated maximum vector element, or a 0, when no exceeding has happend.}
  \item{sigma_factor}{Give back the given sigma\_factor}
  \item{meanofvector}{Give back the calculated mean of the measurement vector}
  \item{sdofvector}{Give back the calculated standard deviation of the measurement vector}
}
\references{Meek, D.W., Hatfield, J.L. (1994) Data quality checking for single station meteorological databases, \_Agricultural and Forest Meteorology\_, *69* (1-2), 85-109.}
\author{Andre Duesterhus}
\seealso{\code{\link{qat_analyse_lim_rule_sigma_1d}}, \code{\link{qat_plot_lim_rule_sigma_2d}}, \code{\link{qat_call_lim_rule}}, \code{\link{qat_analyse_lim_rule_static_2d}}, \code{\link{qat_analyse_lim_rule_dynamic_2d}}}
\examples{
vec <- array(rnorm(100),c(5,20))
result <- qat_analyse_lim_rule_sigma_2d(vec, 2)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ts}