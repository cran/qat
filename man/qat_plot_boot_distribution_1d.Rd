\name{qat_plot_boot_distribution_1d}
\alias{qat_plot_boot_distribution_1d}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plot a bootstrapped distribution check result}
\description{
A plot of the result of a booted distribution check will be produced.
}
\usage{
qat_plot_boot_distribution_1d(resultlist_stat, filename, bootruns = -1, 
measurement_name = "", directoryname = "", plotstyle = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{resultlist_stat}{List of results from qat\_analyse\_boot\_distribution\_1d}
  \item{filename}{Name of the file without extension.}
  \item{bootruns}{Number of bootstrap runs used in the test.}
  \item{measurement_name}{Name of the measurement.}
  \item{directoryname}{Directory, where the resulted file should be stored.}
  \item{plotstyle}{A list with a qat color scheme.}
}
\details{
A plot will be produced, which base on the resulting vectors of qat\_analyse\_boot\_distribution\_1d. When no plotstyle is defined the standard-colorscheme will be used. The resulting plot will be stored in the folder, which is defined by directory under the given filename, with the extension png.
}
\value{
No return value.
}
\author{Andre Duesterhus}
\seealso{\code{\link{qat_analyse_boot_distribution_1d}}}
\examples{
vec <- rnorm(500)
result <- qat_analyse_boot_distribution_1d(vec, 100)
# this example produce a file exampleplot_bootdist.png in the current directory
qat_plot_boot_distribution_1d(result$stat, "exampleplot_bootdist", 
bootruns=result$bootruns, measurement_name="Result of Check")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ts}