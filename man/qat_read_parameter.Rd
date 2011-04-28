\name{qat_read_parameter}
\alias{qat_read_parameter}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Informations on a method}
\description{
This functions delivers informations of methods, which are stored under the given filename.
}
\usage{
qat_read_parameter(filename, methodname)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filename}{Filename of the file with the descriptions of the methods}
  \item{methodname}{Name of the method, where informations are required.}
}
\details{
This functions delivers informations of methods, which are stored under the given filename. For this the methodname will be used as a search parameter. The informations will be given back as a list.
}
\value{
A list with the following elements:
  \item{name}{Name of the method, may be corrected to standard name.}
  \item{analysis_function}{Name of the analysis function, which should be called for this method}
  \item{plot_function}{Name of the plot function, which should be called for this method}
  \item{manipulation_function}{Name of the manipulation function, which should be called for this method}
  \item{description}{Description of the method}
  \item{algorithm}{Algorithm of the method}
}
\author{Andre Duesterhus}

\examples{
#still to come
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}
\keyword{IO}