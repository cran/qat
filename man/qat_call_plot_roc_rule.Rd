\name{qat_call_plot_roc_rule}
\alias{qat_call_plot_roc_rule}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plot a result of a ROC rule check}
\description{
A result of qat\_analyse\_roc\_rule\_static\_1d or qat\_analyse\_roc\_rule\_dynamics\_1d will be plotted.
}
\usage{
qat_call_plot_roc_rule(resultlist_part, measurement_vector = NULL, time = NULL, height = NULL, lat = NULL, lon = NULL, measurement_name = "", directoryname = "", basename = "", plotstyle = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{resultlist_part}{A list with the result, which is directly or indirectly produced by qat\_analyse\_roc\_rule\_static\_1d or qat\_analyse\_roc\_rule\_dynamics\_1d.}
  \item{measurement_vector}{The measurement vector, which was tested}
  \item{time}{A vector of time elements with the length of the measurement vector}
  \item{height}{A vector of height elements with the length of the measurement vector}
  \item{lat}{A vector of latitude elements with the length of the measurement vector}
  \item{lon}{A vector of longitude elements with the length of the measurement vector}
  \item{measurement_name}{Name of the data, which will be used as an indicator in the plot}
  \item{directoryname}{Definition of the directory, where the plot should be stored}
  \item{basename}{Basic name of the resulting file}
  \item{plotstyle}{A list with a qat color scheme}
}
\details{
A plot will be produced, which base on the resulting list of qat\_analyse\_roc\_rule\_static\_1d or qat\_analyse\_roc\_rule\_dynamics\_1d. The measurement\_name will be used as a title of the plot and the plotstyle list define the colors of the plot. When no plotstyle is defined the standard-colorscheme will be used. The resulting plot will be stored in the folder, which is defined by directory. As a filename the basename with additional information will be used (number of test and a label, which indicate which test was performed).
}
\value{
No return value.
}
\author{Andre Duesterhus}
\seealso{\code{\link{qat_plot_roc_rule_dynamic_1d}}, \code{\link{qat_plot_roc_rule_static_1d}}}
\examples{
vec <- rnorm(100)
downward_vector<-seq(1,2,length.out=100)
upward_vector<-seq(1,2,length.out=100)
workflowlist_part <- list(downward_value=2, upward_value=2,downward_vector="vec1",upward_vector="vec2",downward_vector_name="downward vector", upward_vector_name="upward vector")
resultlist <- qat_call_roc_rule(vec, workflowlist_part, element=1, vec1=downward_vector, vec2=upward_vector)
# this example produce the files exampleplot_1_roc_static.png and exampleplot_1_roc_dynamic.png in the current directory
for (ii in 2:3) {
	qat_call_plot_roc_rule(resultlist[[ii]], measurement_vector=vec, measurement_name="Result of Check", basename="exampleplot")
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}