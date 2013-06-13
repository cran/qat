\name{qat_measure_histogram_difference}
\alias{qat_measure_histogram_difference}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Perform a comparison of two datasets by means of its histograms with a given metric}
\description{This function compares two datasets by calculating their histograms and compares them by a given metric.}
\usage{
qat_measure_histogram_difference(data1, data2, metric="EMD", breakvector=NULL, 
numofbars=65, factorofbar=100)
}
\arguments{
  \item{data1}{The first dataset.}
  \item{data2}{The second dataset.}
  \item{metric}{Metric of the comparison. Details see below.}
  \item{breakvector}{Breakvector for the histograms. When not given (NULL), an equidistant breakvector between the minimum and maximum of the two datasets with the given number of bars will be generated.}
  \item{numofbars}{Number of bins of the histogram, when no breakvector is given.}
  \item{factorofbar}{Correction factor for non-value bins.}
}
\details{For both datasets the histograms are computed and compared by means of a given metric. As a metric for the comparison one of the following five options are usable:
EMD: Earth Mover's Distance (default); 
KLD: Kullback-Leibler Distance; 
JSD: Jenson-Shannon Distance; 
RMS: Root Mean Square; 
MS: Mean Square. 
As a result the distance between the two histograms calculated by the metric is given.
}
\references{Duesterhus, A., Hense, A. (2012) Advanced Information Criterion for Environmental Data Quality Assurance, \_Advances in Science and Research\_, *8*, 99-104.}
\author{Andre Duesterhus}
\seealso{\code{\link{qat_analyse_histogram_test_1d}}, \code{\link{qat_analyse_histogram_test_2d}}}
\examples{
vec1 <- array(rnorm(1000), c(100, 20))
vec2 <- vec1 + 1
result <- qat_measure_histogram_difference(vec1, vec2, metric="EMD", numofbars=65)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ts}