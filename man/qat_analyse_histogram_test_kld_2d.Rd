\name{qat_analyse_histogram_test_kld_2d}
\alias{qat_analyse_histogram_test_kld_2d}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Perform a histogram test with the metric KLD}
\description{This check divides the data into blocks, estimates their probability density functions by histograms and compares them by using the Kullback-Leibler Divergence.}
\usage{
qat_analyse_histogram_test_kld_2d(measurement_vector, blocksize = 
floor(length(measurement_vector)/20), numofbars = 65, factorofbar = 100)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{measurement_vector}{The measurement vector (2d array), which should be tested}
  \item{blocksize}{Number of elements in the first dimension, which should be used for each block}
  \item{numofbars}{Number of bins of the histogram}
  \item{factorofbar}{Correction factor for non-value bins}
}
\details{The field will be divided into blocks in the first dimension, with a length given by the parameter blocksize. From these blocks histograms are computed and afterwards compared. As a metric for the comparison the Kullback-Leibler Divergence is used. As a result a field is generated, which includes the results of the comparison between every combination of blocks.}
\value{It returns a list with the following entries:
 \item{field}{Result matrix of the comparison.}
 \item{blocksize}{Size of blocks in the first dimension.}
 \item{numofbars}{Number of bins of the used histograms.}
 \item{factorofbar}{Correction factor used for the used histograms.}
 \item{metric}{Used metric in the comparisons.}
 \item{runs}{Number of blocks, which are compared.}
}
\references{Duesterhus, A., Hense, A. (2012) Advanced Information Criterion for Environmental Data Quality Assurance, \_Advances in Science and Research\_, *8*, 99-104.}
\author{Andre Duesterhus}
\seealso{\code{\link{qat_analyse_histogram_test_kld_1d}}, \code{\link{qat_analyse_histogram_test_jsd_2d}}, \code{\link{qat_analyse_histogram_test_rms_2d}}, \code{\link{qat_analyse_histogram_test_ms_2d}}, \code{\link{qat_analyse_histogram_test_emd_2d}}}
\examples{
vec <- array(rnorm(1000), c(100, 20))
vec[51:100, ] <- round(vec[51:100, ])
result <- qat_analyse_histogram_test_kld_2d(vec, 4, 65, 100)
qat_plot_histogram_test(result$field, "test_kld_2d", result$blocksize, 
result$numofbars, result$factorofbar, "kld", result$runs)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ts}