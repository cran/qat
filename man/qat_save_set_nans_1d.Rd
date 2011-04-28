\name{qat_save_set_nans_1d}
\alias{qat_save_set_nans_1d}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Produce a savelist from a resultlist for a Set NAN
}
\description{
This function takes the results, produced by qat\_analyse\_set\_nan\_1d and construct a savelist, which may be used to produce a netCDF output.
}
\usage{
qat_save_set_nans_1d(resultlist_part, baseunit = "")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{resultlist_part}{A list with the results of the check}
  \item{baseunit}{The unit of the original measurement vector}
}
\details{
This function takes the resultlist and transfer the content to a newly organized list. This consists mainly of a text, which is use in the parameter description for a netCDF-file.
}
\value{
Returning a savelist with the content of the resultlist.
}
\author{Andre Duesterhus}
\seealso{
\code{\link{qat_call_save_set_nans}}
}
\examples{
## still to come
}
\keyword{utilities}