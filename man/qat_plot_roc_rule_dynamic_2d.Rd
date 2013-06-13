\name{qat_plot_roc_rule_dynamic_2d}
\alias{qat_plot_roc_rule_dynamic_2d}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plot a dynamic ROC rule result}
\description{
A plot of the result of a dynamic ROC rule check will be produced.
}
\usage{
qat_plot_roc_rule_dynamic_2d(flagvector, filename, measurement_vector = NULL,
max_upward_vector = NULL, max_downward_vector = NULL, upward_vector_name = NULL, 
downward_vector_name = NULL, measurement_name = "", directoryname = "", 
plotstyle = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{flagvector}{The resulting flagvector of qat\_analyse\_roc\_rule\_dynamic\_2d}
  \item{filename}{Name of the file without extension.}
  \item{measurement_vector}{The measurement vector, which should be plotted}
  \item{max_upward_vector}{The vector (2d array) with the upward values.}
  \item{max_downward_vector}{The vector (2d array) with the downward values.}
  \item{upward_vector_name}{Name of the vector of the upward values. }
  \item{downward_vector_name}{Name of the vector of the downward values. }
  \item{measurement_name}{Name of the measurement.}
  \item{directoryname}{Directory, where the resulted file should be stored.}
  \item{plotstyle}{A list with a qat color scheme.}
}
\details{
A plot will be produced, which base on the resulting flagvector of qat\_analyse\_roc\_rule\_dynamic\_2d. Additional information on the parameters, which were used while performing the test, will be added to the plot. When no plotstyle is defined the standard-colorscheme will be used. The resulting plot will be stored in the folder, which is defined by directory under the given filename, with the extension png.
}
\value{
No return value.
}
\author{Andre Duesterhus}
\seealso{\code{\link{qat_plot_roc_rule_dynamic_1d}}, \code{\link{qat_analyse_roc_rule_dynamic_2d}}, \code{\link{qat_plot_roc_rule_static_2d}}}
\examples{
vec <- array(rnorm(500), c(25,20))
min_vector <- array(rnorm(500)+2, c(25,20))
max_vector <- array(rnorm(500)+2, c(25,20))
result <- qat_analyse_roc_rule_dynamic_2d(vec, min_vector, max_vector, 
upward_vector_name="upward vector", downward_vector_name="downward vector")
# this example produce a file exampleplot_roc_dyn.png in the current directory
qat_plot_roc_rule_dynamic_2d(result$flagvector, "exampleplot_roc_dyn", 
measurement_vector=vec, max_upward_vector=result$max_upward_vector, 
max_downward_vector=result$max_downward_vector, upward_vector_name=result$upward_vector_name, 
downward_vector_name=result$downward_vector_name, measurement_name="Result of Check") 
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ts}