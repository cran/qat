\name{qat_plot_roc_rule_static_2d}
\alias{qat_plot_roc_rule_static_2d}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plot a static ROC rule result}
\description{
A plot of the result of a static ROC rule check will be produced.
}
\usage{
qat_plot_roc_rule_static_2d(flagvector, filename, measurement_vector = NULL, 
max_upward_value = 0, max_downward_value = 0, measurement_name = "", 
directoryname = "", plotstyle = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{flagvector}{The resulting flagvector of qat\_analyse\_roc\_rule\_static\_2d}
  \item{filename}{Name of the file without extension.}
  \item{measurement_vector}{The measurement vector, which should be plotted}
  \item{max_upward_value}{The used maximum upward value.}
  \item{max_downward_value}{The used maximum downward value.}
  \item{measurement_name}{Name of the measurement.}
  \item{directoryname}{Directory, where the resulted file should be stored.}
  \item{plotstyle}{A list with a qat color scheme.}
}
\details{
A plot will be produced, which base on the resulting flagvector of qat\_analyse\_roc\_rule\_static\_2d. Additional information on the parameters, which were used while performing the test, will be added to the plot. When no plotstyle is defined the standard-colorscheme will be used. The resulting plot will be stored in the folder, which is defined by directory under the given filename, with the extension png.
}
\value{
No return value.
}
\author{Andre Duesterhus}
\seealso{\code{\link{qat_plot_roc_rule_static_1d}}, \code{\link{qat_analyse_roc_rule_static_2d}}, \code{\link{qat_plot_roc_rule_dynamic_2d}}}
\examples{
vec <- array(rnorm(500), c(25,20))
result <- qat_analyse_roc_rule_static_2d(vec, 2,2)
# this example produce a file exampleplot_roc_sta.png in the current directory
qat_plot_roc_rule_static_2d(result$flagvector, "exampleplot_roc_sta", 
measurement_vector=vec, max_upward_value=result$max_upward_value, 
max_downward_value=result$max_downward_value, measurement_name="Result of Check") 
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ts}