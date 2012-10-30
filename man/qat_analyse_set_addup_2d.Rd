\name{qat_analyse_set_addup_2d}
\alias{qat_analyse_set_addup_2d}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Addup values of a vector}
\description{
This function adds up successive values of a given vector
}
\usage{
qat_analyse_set_addup_2d(measurement_vector, blocksize)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{measurement_vector}{The measurement vector, which should be tested}
  \item{blocksize}{Number of elements, which should be added up}
}
\details{
Starting with the first element the measurement vector will be split up into blocks of the size of the parameter block size. In a second step the elements of these blocks will be summed up. If the last block haven't the size of block size, this block will be ignored.
}
\value{
Give back a list, which includes the vector with the results of the blocks.
}
\author{Andre Duesterhus}
\seealso{\code{\link{qat_analyse_set_mean_2d}}, \code{\link{qat_analyse_set_nans_2d}}}
\examples{
vec <- array(rnorm(144), c(12,12))
result <- qat_analyse_set_addup_2d(vec, 3)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ts}
\keyword{manip}