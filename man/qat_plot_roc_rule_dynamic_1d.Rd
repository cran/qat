\name{qat_plot_roc_rule_dynamic_1d}
\alias{qat_plot_roc_rule_dynamic_1d}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plot a dynamic ROC rule result}
\description{
A plot of the result of a dynamic ROC rule check will be produced.
}
\usage{
qat_plot_roc_rule_dynamic_1d(flagvector, filename, measurement_vector = NULL, 
max_upward_vector = NULL, max_downward_vector = NULL, upward_vector_name = NULL, 
downward_vector_name = NULL, measurement_name = "", directoryname = "", 
plotstyle = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{flagvector}{The resulting flagvector of qat\_analyse\_roc\_rule\_dynamic\_1d}
  \item{filename}{Name of the file without extension.}
  \item{measurement_vector}{The measurement vector, which should be plotted}
  \item{max_upward_vector}{The vector with the upward values.}
  \item{max_downward_vector}{ The vector with the downward values.}
  \item{upward_vector_name}{Name of the vector of the upward values. }
  \item{downward_vector_name}{Name of the vector of the downward values. }
  \item{measurement_name}{Name of the measurement.}
  \item{directoryname}{Directory, where the resulted file should be stored.}
  \item{plotstyle}{A list with a qat color scheme.}
}
\details{
A plot will be produced, which base on the resulting flagvector of qat\_analyse\_roc\_rule\_dynamic\_1d. With additional information on the parameters, which were used while performing the test, this function will produce a more detailed plot. When no plotstyle is defined the standard-colorscheme will be used. The resulting plot will be stored in the folder, which is defined by directory under the given filename, with the extension png.
}
\value{
No return value.
}
\author{Andre Duesterhus}
\seealso{\code{\link{qat_analyse_roc_rule_dynamic_1d}},\code{\link{qat_plot_roc_rule_static_1d}}}
\examples{
vec <- rnorm(100)
min_vector<-seq(1,2,length.out=100)
max_vector<-seq(1,2,length.out=100)
result <- qat_analyse_roc_rule_dynamic_1d(vec, min_vector, max_vector, 
upward_vector_name="upward vector", downward_vector_name="downward vector")
# this example produce a file exampleplot_roc_dyn.png in the current directory
qat_plot_roc_rule_dynamic_1d(result$flagvector, "exampleplot_roc_dyn", 
measurement_vector=vec, max_upward_vector=result$max_upward_vector, 
max_downward_vector=result$max_downward_vector, upward_vector_name=result$upward_vector_name, 
downward_vector_name=result$downward_vector_name, measurement_name="Result of Check") 
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ts}