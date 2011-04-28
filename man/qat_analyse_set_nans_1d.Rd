\name{qat_analyse_set_nans_1d}
\alias{qat_analyse_set_nans_1d}
\title{Set given values of a vector to NaN}
\description{
This function set a specified value of a vector to NaN.
}
\usage{
qat_analyse_set_nans_1d(measurement_vector, nan_value)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{measurement_vector}{The measurement vector, which should be worked on}
  \item{nan_value}{Value, which should be replaced by NaN}
}
\details{
In the given measurement vector, the value, which is specified by nan\_value, will be replaced by NaN.
}
\value{
Retrun a list, which includes the measurement vector with the replaced values.
}
\author{Andre Duesterhus}
\seealso{\code{\link{qat_analyse_set_addup_1d}}, \code{\link{qat_analyse_set_mean_1d}}}
\examples{
vec <- c(1,2,3,4,5,4,3,2,1)
result <- qat_analyse_set_nans_1d(vec, 4)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ts}
\keyword{manip}