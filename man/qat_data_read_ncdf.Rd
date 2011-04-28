\name{qat_data_read_ncdf}
\alias{qat_data_read_ncdf}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Read in netCDF-file}
\description{
A netCDF file will be read in and a ncdf-object will be given back.
}
\usage{
qat_data_read_ncdf(filename)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filename}{Path and filename of the netCDF-file, which should be read in.}
}
\value{
A ncdf-Object, with the content of the file.
}
\author{Andre Duesterhus}
\seealso{\code{\link{qat_data_nameofvars_ncdf}}, \code{\link{qat_data_numofvars_ncdf}}, \code{\link{qat_data_varcontent_ncdf}}}
\examples{
#still to come
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{IO}