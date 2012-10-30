\name{qat_plot_block_distribution_2d}
\alias{qat_plot_block_distribution_2d}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plot a block distribution check result}
\description{
A plot of the result of a block distribution check will be produced.
}
\usage{
qat_plot_block_distribution_2d(resultlist, filename, blocksize = -1, measurement_name = "", directoryname = "", plotstyle = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{resultlist}{List of results from qat\_analyse\_block\_distribution\_2d}
  \item{filename}{Name of the file without extension.}
  \item{blocksize}{Length of the blocks}
  \item{measurement_name}{Name of the measurement.}
  \item{directoryname}{Directory, where the resulted file should be stored.}
  \item{plotstyle}{A list with a qat color scheme.}
}
\details{
A plot will be produced, which base on the resulting flagvector of qat\_analyse\_block\_distribution\_2d. Additional information on the parameters, which were used while performing the test, will be included into the plot. When no plotstyle is defined the standard-colorscheme will be used. The resulting plot will be stored in the folder, which is defined by directory under the given filename, with the extension png.
}
\value{
No return value.
}
\author{Andre Duesterhus}
\seealso{\code{\link{qat_analyse_block_distribution_1d}}}
\examples{
vec <- array(rnorm(500),c(25,20))
result <- qat_analyse_block_distribution_2d(vec, 5)
# this example produce a file exampleplot_blockdist.png in the current directory
qat_plot_block_distribution_2d(result$stat, "exampleplot_blockdist", blocksize=result$blocksize, measurement_name="Result of Check")		
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ts}