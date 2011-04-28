\name{qat_data_numofvars_ncdf}
\alias{qat_data_numofvars_ncdf}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Nomber of Variables of ncdf object}
\description{
Give back the number of the variables in a netCDF-object.
}
\usage{
qat_data_numofvars_ncdf(obj)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{A netcdf object, which will be produced by qat\_data\_read\_ncdf.}
}
\details{
The number of variables, which are stored in the ncdf-obect will be given back.
}
\value{
Number of variables.
}
\author{Andre Duesterhus}
\seealso{\code{\link{qat_data_read_ncdf}}, \code{\link{qat_data_nameofvars_ncdf}}
\code{\link{qat_data_varcontent_ncdf}}}
\examples{
#still to come
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}