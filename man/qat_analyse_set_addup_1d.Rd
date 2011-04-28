\name{qat_analyse_set_addup_1d}
\alias{qat_analyse_set_addup_1d}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Addup values of a vector}
\description{
This function adds up sucessive values of a given vector
}
\usage{
qat_analyse_set_addup_1d(measurement_vector, blocksize)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{measurement_vector}{The measurement vector, which should be tested}
  \item{blocksize}{Number of elements, which should be added up}
}
\details{
Starting with the first element the measurement vector will be splitted up into blocks of the size of the parameter block size. In a second step the elements of these blocks will be summed up. If the last block haven't the size of block size, this block will be ignored.
}
\value{
Give back a list, which includes the vector with the results of the blocks.
}
\author{Andre Duesterhus}
\seealso{\code{\link{qat_analyse_set_mean_1d}}, \code{\link{qat_analyse_set_nans_1d}}}
\examples{
vec <- c(1,2,3,4,5,4,3,2,1)
result <- qat_analyse_set_addup_1d(vec, 3)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ts}
\keyword{manip}