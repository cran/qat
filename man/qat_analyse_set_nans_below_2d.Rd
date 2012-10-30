\name{qat_analyse_set_nans_below_2d}
\alias{qat_analyse_set_nans_below_2d}
\title{Set values below threshold to NaN}
\description{
This function set a values of a vector below a given value to NaN.
}
\usage{
qat_analyse_set_nans_below_2d(measurement_vector, nan_below)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{measurement_vector}{The measurement vector, which should be worked on}
  \item{nan_below}{Value, below the values should be replaced by NaN}
}
\details{
In the given measurement vector, the values, which are below nan\_below, will be replaced by NaN.
}
\value{
Return a list, which includes the measurement vector with the replaced values.
}
\author{Andre Duesterhus}
\seealso{\code{\link{qat_analyse_set_addup_2d}}, \code{\link{qat_analyse_set_mean_2d}}, \code{\link{qat_analyse_set_nans_2d}}, \code{\link{qat_analyse_set_nans_above_2d}}}
\examples{
vec <- array(c(1,2,3,4,5,4,3,2,1), c(3,3))
result <- qat_analyse_set_nans_below_2d(vec, 4)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ts}
\keyword{manip}