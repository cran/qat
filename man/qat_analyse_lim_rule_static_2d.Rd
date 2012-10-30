\name{qat_analyse_lim_rule_static_2d}
\alias{qat_analyse_lim_rule_static_2d}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Perform a static lim-rule-check}
\description{
 This check tests data on whether it exceeds a static threshold.
}
\usage{
qat_analyse_lim_rule_static_2d(measurement_vector, min_value, max_value)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{measurement_vector}{The measurement vector (2d array), which should be tested}
  \item{min_value}{The minimum threshold}
  \item{max_value}{The maximum threshold}
}
\details{
This check tests every element, on whether it exceeds the minimum or maximum threshold. The result will be given back as a list, which contains the result of the test as a flagvector (2d array) and its parameters. For every element of the measurement vector the flagvector contains a -1, if its exceeding the minimum value, a 1, if its exceeding the maximum value, or a 0, when no exceeding has happend. 
}
\value{
It returns a list with the following entries:
  \item{flagvector}{A vector of length of measurement vector. For every element of the measurement vector the flagvector contains a -1, if its exceeding the minimum value, a 1, if its exceeding the maximum value, or a 0, when no exceeding has happend.}
  \item{min_value}{Give back the given min\_value}
  \item{max_value}{Give back the given max\_value}
}
\references{Meek, D.W., Hatfield, J.L. (1994) Data quality checking for single station meteorological databases, \_Agricultural and Forest Meteorology\_, *69* (1-2), 85-109.}
\author{Andre Duesterhus}

\section{Warning }{There is no checking, if the maximum-value is greater than the minimum-value.}

\seealso{\code{\link{qat_analyse_lim_rule_static_1d}}, \code{\link{qat_plot_lim_rule_static_2d}}, \code{\link{qat_call_lim_rule}}, \code{\link{qat_analyse_lim_rule_dynamic_2d}}, \code{\link{qat_analyse_lim_rule_sigma_2d}}}
\examples{
vec <- array(rnorm(100),c(5,20))
result <- qat_analyse_lim_rule_static_2d(vec, -2,2)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ts}