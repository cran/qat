\name{qat_save_lim_rule_dynamic_1d}
\alias{qat_save_lim_rule_dynamic_1d}
\title{Produce a savelist from a resultlist for a LIM Rule Dynamic Test}
\description{
This function takes the results, produced by qat\_analyse\_lim\_rule\_dynamic\_1d and construct a savelist, which may be used to produce a netCDF output.}
\usage{
qat_save_lim_rule_dynamic_1d(resultlist_part, baseunit = "")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{resultlist_part}{A list with the results of the check}
  \item{baseunit}{The unit of the original measurement vector}
}
\details{
This function takes the resultslist and transfer the content to a newly organized list. Ths also consists of more information, which help to generate an output like a netCDF-file.}
\value{
Returning a savelist with the content of the resultlist.
}
\author{Andre Duesterhus}
\seealso{\code{\link{qat_call_save_lim_rule}}, \code{\link{qat_run_workflow_save}}}
\examples{
	vec <- rnorm(1000)
	min_vector<-seq(-1,-2,length.out=1000)
	max_vector<-seq(1,2,length.out=1000)
	result <- list(result=qat_analyse_lim_rule_dynamic_1d(vec, min_vector, 
	max_vector, min_vector_name="minimum vector", max_vector_name="maximum vector"))
	savelist <- qat_save_lim_rule_dynamic_1d(result)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}