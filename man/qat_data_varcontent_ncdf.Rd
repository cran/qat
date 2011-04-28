\name{qat_data_varcontent_ncdf}
\alias{qat_data_varcontent_ncdf}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Content of a variable}
\description{
Give back the content of a specified variable of an ncdf-object.
}
\usage{
qat_data_varcontent_ncdf(obj, numofvar)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{A netcdf object, which will be produced by qat\_data\_read\_ncdf.}
  \item{numofvar}{Number of variable, which content should be delivered.}
}
\details{
The content of the variable, which is specified by its number in numofvars will be given back.
}
\value{
The content of the variable.
}
\author{Andre Duesterhus}
\seealso{\code{\link{qat_data_read_ncdf}}, \code{\link{qat_data_nameofvars_ncdf}}
\code{\link{qat_data_numofvars_ncdf}}}
\examples{
#still to come
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}