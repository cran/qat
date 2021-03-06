\name{qat_plot_noc_rule_1d}
\alias{qat_plot_noc_rule_1d}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plot a NOC rule result}
\description{
A plot of the result of a NOC rule check will be produced.
}
\usage{
qat_plot_noc_rule_1d(flagvector, filename, measurement_vector = NULL, 
max_return_elements = 0, measurement_name = "", directoryname = "", 
plotstyle = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{flagvector}{The resulting flagvector of qat\_analyse\_noc\_rule\_1d}
  \item{filename}{Name of the file without extension.}
  \item{measurement_vector}{The measurement vector, which should be plotted}
  \item{max_return_elements}{The number of maximum reruning elements, which was used in the test.}
  \item{measurement_name}{Name of the measurement.}
  \item{directoryname}{Directory, where the resulted file should be stored.}
  \item{plotstyle}{A list with a qat color scheme.}
}
\details{
A plot will be produced, which base on the resulting flagvector of qat\_analyse\_noc\_rule\_1d. With additional information on the parameters, which were used while performing the test, this function will produce a more detailed plot. When no plotstyle is defined the standard-colorscheme will be used. The resulting plot will be stored in the folder, which is defined by directory under the given filename, with the extension png.
}
\value{
No return value.
}
\author{Andre Duesterhus}
\seealso{\code{\link{qat_analyse_noc_rule_1d}}}
\examples{
vec <- c(1,2,3,4,4,4,5,5,4,3,NaN,3,2,1)
result <- qat_analyse_noc_rule_1d(vec, 1)
# this example produce a file exampleplot_noc.png in the current directory
qat_plot_noc_rule_1d(result$flagvector, "exampleplot_noc", measurement_vector=vec, 
max_return_elements=result$max_return_elements, measurement_name="Result of Check")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ts}