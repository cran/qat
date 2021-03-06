\name{qat_analyse_set_nans_below_1d}
\alias{qat_analyse_set_nans_below_1d}
\title{Set values below threshold to NaN}
\description{
This function set a values of a vector below a given value to NaN.
}
\usage{
qat_analyse_set_nans_below_1d(measurement_vector, nan_below)
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
\seealso{\code{\link{qat_analyse_set_addup_1d}}, \code{\link{qat_analyse_set_mean_1d}}, \code{\link{qat_analyse_set_nans_1d}}, \code{\link{qat_analyse_set_nans_above_1d}}}
\examples{
vec <- c(1,2,3,4,5,4,3,2,1)
result <- qat_analyse_set_nans_below_1d(vec, 4)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ts}
\keyword{manip}