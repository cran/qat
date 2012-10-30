\name{qat_plot_lim_rule_dynamic_2d}
\alias{qat_plot_lim_rule_dynamic_2d}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plot a dynamic LIM rule result}
\description{
A plot of the result of a dynamic LIM rule check will be produced.
}
\usage{
qat_plot_lim_rule_dynamic_2d(flagvector, filename, measurement_vector = NULL, min_vector = NULL, max_vector = NULL, min_vector_name = NULL, max_vector_name = NULL, measurement_name = "", directoryname = "", plotstyle = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{flagvector}{The resulting flagvector of qat\_analyse\_lim\_rule\_dynamic\_2d}
  \item{filename}{Name of the file without extension.}
  \item{measurement_vector}{The measurement vector, which should be plotted}
  \item{min_vector}{The vector (2d array) with the minimum values.}
  \item{max_vector}{The vector (2d array) with the maximum values.}
  \item{min_vector_name}{Name of the vector of the minimum values.}
  \item{max_vector_name}{Name of the vector of the minimal values.}
  \item{measurement_name}{Name of the measurement.}
  \item{directoryname}{Directory, where the resulted file should be stored.}
  \item{plotstyle}{A list with a qat color scheme.}
}
\details{
A plot will be produced, which base on the resulting flagvector of qat\_analyse\_lim\_rule\_dynamic\_2d. Additional information on the parameters, which were used while performing the test, will be added to the plot. When no plotstyle is defined the standard-colorscheme will be used. The resulting plot will be stored in the folder, which is defined by directory under the given filename, with the extension png.
}
\value{
No return value.
}
\author{Andre Duesterhus}
\seealso{\code{\link{qat_plot_lim_rule_dynamic_1d}}, \code{\link{qat_analyse_lim_rule_dynamic_2d}}, \code{\link{qat_plot_lim_rule_static_2d}}, \code{\link{qat_plot_lim_rule_sigma_2d}}}
\examples{
vec <- array(rnorm(500),c(25,20))
min_vector <- array(rnorm(500)-2, c(25,20))
max_vector <- array(rnorm(500)+2, c(25,20))
result <- qat_analyse_lim_rule_dynamic_2d(vec, min_vector, max_vector, min_vector_name="minimum vector", max_vector_name="maximum vector")
# this example produce a file exampleplot_lim_dyn.png in the current directory
qat_plot_lim_rule_dynamic_2d(result$flagvector, "exampleplot_lim_dyn", measurement_vector=vec, min_vector=result$min_vector, max_vector=result$max_vector, min_vector_name=result$min_vector_name, max_vector_name=result$max_vector_name, measurement_name="Result of Check")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ts}