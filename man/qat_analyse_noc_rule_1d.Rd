\name{qat_analyse_noc_rule_1d}
\alias{qat_analyse_noc_rule_1d}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Perform a noc-rule-check}
\description{
 This check tests data on whether it changes after a given amount of values.
}
\usage{
qat_analyse_noc_rule_1d(measurement_vector, max_return_elements)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{measurement_vector}{The measurement vector, which should be tested}
  \item{max_return_elements}{Number of coherent elements, which are allowed to have no change between the single values, without indicate an error}
}
\details{
This check tests the given measurement vector from the beginning to the end, on how much values in a row got the same value. If the number of values, which is defined by max\_return\_elements prior to the actual element got the same value as the actual element, the resulting flagvector will be set to 1 on the actual position. Else it will be set to 0. 
}
\value{
It returns a list with the following entries:
  \item{flagvector}{flagvektor with the dimension of measurement vector, where a 0 indicates no error and a 1 that there is a repetition error}
  \item{max_return_elements}{Give back the given max\_return\_elements}
}
\references{Meek, D.W., Hatfield, J.L. (1994) Data quality checking for single station meteorological databases, \_Agricultural and Forest Meteorology\_, *69* (1-2), 85-109.}
\author{Andre Duesterhus}
\seealso{\code{\link{qat_plot_noc_rule_1d}}, \code{\link{qat_call_noc_rule}}}
\examples{
vec <- c(1,2,3,4,4,4,5,5,4,3,NaN,3,2,1)
result <- qat_analyse_noc_rule_1d(vec, 1)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ts}
