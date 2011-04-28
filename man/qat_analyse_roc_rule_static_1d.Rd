\name{qat_analyse_roc_rule_static_1d}
\alias{qat_analyse_roc_rule_static_1d}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Perform a static roc-rule-check}
\description{
This check tests data on whether the change between two consecutive data points exceeds a static threshold.
}
\usage{
qat_analyse_roc_rule_static_1d(measurement_vector, max_upward_value, max_downward_value)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{measurement_vector}{The measurement vector, which should be tested}
  \item{max_upward_value}{The upward threshold}
  \item{max_downward_value}{The downward threshold, which should be positive definite}
}
\details{
This check tests two consecutive elements, on wether the change of values between those two exceeds the upward or downward threshold. The result will be given back as a list, which contains the result of the test as a flagvector and its parameters. For every change between two elements of the measurement vector the flagvector contains a -1, if its exceeding the downward value, a 1, if its exceeding the upward value, or a 0, when no exceeding has happend. 
}
\value{
It returns a list with the following entries:
  \item{flagvector}{A vector of length of measurement vector.  For every change between two elements of the measurement vector the flagvector contains a -1, if its exceeding the downward value, a 1, if its exceeding the upward value, or a 0, when no exceeding has happend.}
  \item{max_upward_value}{Give back the given max\_upward\_value}
  \item{max_downward_value}{Give back the given max\_downward\_value}
}
\references{Meek, D.W., Hatfield, J.L. (1994) Data quality checking for single station meteorological databases, \_Agricultural and Forest Meteorology\_, *69* (1-2), 85-109.}
\author{Andre Duesterhus}
\seealso{\code{\link{qat_plot_roc_rule_static_1d}}, \code{\link{qat_call_roc_rule}}, \code{\link{qat_analyse_roc_rule_dynamic_1d}}}
\examples{
vec <- rnorm(100)
result <- qat_analyse_roc_rule_static_1d(vec, 2,2)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ts}