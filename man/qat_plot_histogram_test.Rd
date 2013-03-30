\name{qat_plot_histogram_test}
\alias{qat_plot_histogram_test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plot a histogram test result}
\description{A plot of the result of a histogram test will be produced.}
\usage{
qat_plot_histogram_test(resultfield, filename, blocksize = -1, numofbars = -1, factorofbar = -1, metric = NULL, runs = NULL, measurement_name = "", directoryname = "", plotstyle = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{resultfield}{The resulting matrix of qat\_analyse\_histogram\_test\_xxx\_xd}
  \item{filename}{Name of the file without extension.}
  \item{blocksize}{Length of a block.}
  \item{numofbars}{Number of bins of the histograms.}
  \item{factorofbar}{Correction factor for non-value bins.}
  \item{metric}{Metric used for the comparison of the histograms.}
  \item{runs}{Number of used blocks.}
  \item{measurement_name}{Name of the measurement.}
  \item{directoryname}{Directory, where the resulted file should be stored.}
  \item{plotstyle}{A list with a qat color scheme.}
}
\details{
A plot will be produced, which base on the resulting field of qat\_analyse\_histogram\_test\_xxx\_xd. With additional information on the parameters, which were used while performing the test, this function will produce a more detailed plot. When no plotstyle is defined the standard-colorscheme will be used. The resulting plot will be stored in the folder, which is defined by directory under the given filename, with the extension png.
}
\value{
No return value.
}
\author{Andre Duesterhus}
\examples{
	vec <- c(rnorm(1000), rnorm(1000)+1)
	result <- qat_analyse_histogram_test_emd_1d(vec, 50, 65)
	qat_plot_histogram_test(result$field, "test_emd_1d", result$blocksize, result$numofbars, "emd", result$runs)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ts}