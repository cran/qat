\name{qat_analyse_roc_rule_dynamic_1d}
\alias{qat_analyse_roc_rule_dynamic_1d}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Perform a dynamic roc-rule-check}
\description{
This check tests data on whether the change between two consecutive data points exceeds a dynamic threshold.
}
\usage{
qat_analyse_roc_rule_dynamic_1d(measurement_vector, max_upward_vector = NULL, max_downward_vector = NULL, upward_vector_name = NULL, downward_vector_name = NULL, upward_vector_identifier = NULL, downward_vector_identifier = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{measurement_vector}{The measurement vector, which should be tested}
  \item{max_upward_vector}{A vector which consists of the threshold values for upward changes, with the same dimension like the measurement vector}
  \item{max_downward_vector}{A vector which consists of the threshold values for downward changes, with the same dimension like the measurement vector and have to be positive definite}
  \item{upward_vector_name}{A name or title of the upward vector, which will be given back in the result}
  \item{downward_vector_name}{A name or title of the downward vector, which will be given back in the result}
  \item{upward_vector_identifier}{The identifier of the upward vector}
  \item{downward_vector_identifier}{The identifier of the downward vector}
}
\details{
This check tests two consecutive elements, on wether the change of values between those two exceeds the upward or downward threshold. The result will be given back as a list, which contains the result of the test as a flagvector and its parameters. For every change between two elements of the measurement vector the flagvector contains a -1, if its exceeding its dedicated downward vector element, a 1, if its exceeding its dedicated upward vector element, or a 0, when no exceeding has happend. NaN-values in the measurement vector will be given back as a 0 in the flagvector, NaN-values in the upward or downward-vector are considered as not existing.
}
\value{
It returns a list with the following entries:
  \item{flagvector}{A vector of length of measurement vector. For every change between two elements of the measurement vector the flagvector contains a -1, if its exceeding its dedicated downward vector element, a 1, if its exceeding its dedicated upward vector element, or a 0, when no exceeding has happend.}
  \item{max_upward_vector}{Give back the given max\_upward\_vector}
  \item{max_downward_vector}{Give back the given max\_downward\_vector}
  \item{upward_vector_name}{Give back the given upward\_vector\_name}
  \item{downward_vector_name}{Give back the given downward\_vector\_name}
}
\references{Meek, D.W., Hatfield, J.L. (1994) Data quality checking for single station meteorological databases, \_Agricultural and Forest Meteorology\_, *69* (1-2), 85-109.}
\author{Andre Duesterhus}
\seealso{\code{\link{qat_plot_roc_rule_dynamic_1d}}, \code{\link{qat_call_roc_rule}}, \code{\link{qat_analyse_roc_rule_static_1d}}}
\examples{
vec <- rnorm(100)
min_vector<-seq(1,2,length.out=1000)
max_vector<-seq(1,2,length.out=1000)
result <- qat_analyse_roc_rule_dynamic_1d(vec, min_vector, max_vector, upward_vector_name="upward vector", downward_vector_name="downward vector")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ts}