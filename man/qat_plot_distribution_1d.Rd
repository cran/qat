\name{qat_plot_distribution_1d}
\alias{qat_plot_distribution_1d}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plot a distribution check result}
\description{
A plot of the result of a distribution check will be produced.
}
\usage{
qat_plot_distribution_1d(resultlist_hist, filename, resultlist_stat, 
numofbars = -1, measurement_name = "", directoryname = "", plotstyle = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{resultlist_hist}{Result of a hist function.}
  \item{filename}{Name of the file without extension.}
  \item{resultlist_stat}{List of statistical parameters.}
  \item{numofbars}{Numbers of bars of the histogram plot.}
  \item{measurement_name}{Name of the measurement.}
  \item{directoryname}{Directory, where the resulted file should be stored.}
  \item{plotstyle}{A list with a qat color scheme.}
}
\details{
A plot will be produced, which base on the resulting flagvector of qat\_analyse\_distribution\_1d. Additional information on the parameters, which were used while performing the test, will be included into the plot. When no plotstyle is defined the standard-colorscheme will be used. The resulting plot will be stored in the folder, which is defined by directory under the given filename, with the extension png.

}
\value{
No return value.
}
\author{Andre Duesterhus}
\seealso{\code{\link{qat_analyse_distribution_1d}}}
\examples{
vec <- rnorm(1000)
result <- qat_analyse_distribution_1d(vec, 15)
# this example produce a file exampleplot_dist.png in the current directory
qat_plot_distribution_1d(result$hist, "exampleplot_dist", result$stat, 
numofbars=result$numofbars, measurement_name="Result of Check")	

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ts}