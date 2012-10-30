\name{qat_analyse_lim_rule_dynamic_2d}
\alias{qat_analyse_lim_rule_dynamic_2d}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Perform a dynamic lim-rule-check }
\description{
 This check tests data on whether it exceeds a dynamic threshold.
}
\usage{
qat_analyse_lim_rule_dynamic_2d(measurement_vector, min_vector = NULL, max_vector = NULL, min_vector_name = NULL, max_vector_name = NULL, min_vector_identifier = NULL, max_vector_identifier = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{measurement_vector}{The measurement vector (2d array), which should be tested }
  \item{min_vector}{A 2d array which consists of the minimum threshold values, with the same dimensions like the measurement vector}
  \item{max_vector}{A 2d array which consists of the maximum threshold values, with the same dimensions like the measurement vector}
  \item{min_vector_name}{A name or title of the minimum vector, which will be given back in the result}
  \item{max_vector_name}{A name or title of the maximum vector, which will be given back in the result}
  \item{max_vector_identifier}{The identifier of the maximum vector}
  \item{min_vector_identifier}{The identifier of the minimum vector}
}
\details{
This tests tests every element, on whether it exceeds the minimum or maximum threshold. The result will be given back as a list, which contains the result of the test as a flagvector (2d array) and its parameters. For every element of the measurement vector the flagvector contains a -1, if its exceeding its dedicated minimum vector element, a 1, if its exceeding its dedicated maximum vector element, or a 0, when no exceeding has happend. NaN-values in the measurement vector will be given back as a 0 in the flagvector, NaN-values in the minimum or maximum-vector are considered as not existing. There is no checking, if the maximum-vector is greater than the minimum-vector. 
}
\value{
It returns a list with the following entries:
  \item{flagvector}{A vector of length of measurement vector. For every element of the measurement vector the flagvector contains a -1, if its exceeding its dedicated minimum vector element, a 1, if its exceeding its dedicated maximum vector element, or a 0, when no exceeding has happend.}
  \item{min_vector}{Give back the given min\_vector}
  \item{max_vector}{Give back the given max\_vector}
  \item{min_vector_name}{Give back the given min\_vector\_name}
  \item{max_vector_name}{Give back the given max\_vector\_name}
}
\references{Meek, D.W., Hatfield, J.L. (1994) Data quality checking for single station meteorological databases, \_Agricultural and Forest Meteorology\_, *69* (1-2), 85-109.}
\author{Andre Duesterhus}
\section{Warning }{There is no checking, if the maximum-vector is greater than the minimum-vector.}
\seealso{\code{\link{qat_analyse_lim_rule_dynamic_1d}}, \code{\link{qat_plot_lim_rule_dynamic_2d}}, \code{\link{qat_call_lim_rule}}, \code{\link{qat_analyse_lim_rule_static_2d}}, \code{\link{qat_analyse_lim_rule_sigma_2d}}}
\examples{
vec <- array(rnorm(100),c(5,20))
min_vector<-array(rnorm(100)-2,c(5,20))
max_vector<-array(rnorm(100)+2,c(5,20))
result <- qat_analyse_lim_rule_dynamic_2d(vec, min_vector, max_vector, min_vector_name="minimum vector", max_vector_name="maximum vector")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ts}