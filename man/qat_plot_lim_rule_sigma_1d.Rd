\name{qat_plot_lim_rule_sigma_1d}
\alias{qat_plot_lim_rule_sigma_1d}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plot a sigma LIM rule result}
\description{
A plot of the result of a dynamic lim rule check will be produced.
}
\usage{
qat_plot_lim_rule_sigma_1d(flagvector, filename, measurement_vector = NULL, sigma_factor = NULL, meanofvector = NaN, sdofvector = NULL, measurement_name = "", directoryname = "", plotstyle = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{flagvector}{The resulting flagvector of qat\_analyse\_lim\_rule\_sigma\_1d}
  \item{filename}{Name of the file without extension.}
  \item{measurement_vector}{The measurement vector, which should be plotted}
  \item{sigma_factor}{The sigma factor, which was used, when the test were performed.}
  \item{meanofvector}{The mean of the measurement vector}
  \item{sdofvector}{The standard deviation of the measurement vector}
  \item{measurement_name}{Name of the measurement.}
  \item{directoryname}{Directory, where the resulted file should be stored.}
  \item{plotstyle}{A list with a qat color scheme.}
}
\details{
A plot will be produced, which base on the resulting flagvector of qat\_analyse\_lim\_rule\_sigma\_1d. With additional information on the parameters, which were used while performing the test, this function will produce a more detailed plot. When no plotstyle is defined the standard-colorscheme will be used. The resulting plot will be stored in the folder, which is defined by directory under the given filename, with the extension png.
}
\value{
No return value
}
\author{Andre Duesterhus}
\seealso{\code{\link{qat_analyse_lim_rule_static_1d}},\code{\link{qat_plot_lim_rule_dynamic_1d}}, \code{\link{qat_plot_lim_rule_sigma_1d}}}
\examples{
vec <- rnorm(1000)
result <- qat_analyse_lim_rule_sigma_1d(vec, 2)
# this example produce a file exampleplot_lim_sig.png in the current directory
qat_plot_lim_rule_sigma_1d(result$flagvector, "exampleplot_lim_sig", measurement_vector=vec, sigma_factor=result$sigma_factor, meanofvector=result$meanofvector, sdofvector=result$sdofvector, measurement_name="Result of Check")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ts}