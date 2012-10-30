\name{qat_call_save_lim_rule}
\alias{qat_call_save_lim_rule}
\title{Produce a savelist-entry for a LIM-RULE Test}
\description{
This function calls qat\_save\_lim\_rule\_static\_1d, qat\_save\_lim\_rule\_sigma\_1d or qat\_save\_lim\_rule\_dynamic\_1d. As a result a part of a savelist is constructed, which can be used to construct a netCDF file.}
\usage{
qat_call_save_lim_rule(resultlist_part, element = -999, dim_mv=1, time = NULL, height = NULL, lat = NULL, lon = NULL, vec1 = NULL, vec2 = NULL, vec3 = NULL, vec4 = NULL, baseunit = NULL, savelist = list(), savelistcounter = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{resultlist_part}{A list with the results of the check}
  \item{element}{Element-identifier for the result, which will be given back in the savelist}
  \item{dim_mv}{Dimension of the measurement vector.}
  \item{time}{A vector of time elements with the length of the measurement vector}
  \item{height}{A vector of height elements with the length of the measurement vector}
  \item{lat}{A vector of latitude elements with the length of the measurement vector}
  \item{lon}{A vector of longitude elements with the length of the measurement vector}
  \item{vec1}{An additional vector, which is named as vec1}
  \item{vec2}{An additional vector, which is named as vec2}
  \item{vec3}{An additional vector, which is named as vec3}
  \item{vec4}{An additional vector, which is named as vec4}
  \item{baseunit}{The unit of the original measurement vector}
  \item{savelist}{A list with save elements}
  \item{savelistcounter}{Numbers of elements of the savelist}
}
\details{
This function calls the described saving-function, which transform the resultlist elements to a savinglist element. The possible called functions are qat\_save\_lim\_rule\_static\_1d, qat\_save\_lim\_rule\_sigma\_1d or qat\_save\_lim\_rule\_dynamic\_1d. As a result the given savelist will get an additional entry.
}
\value{
The given savelist will be returned, with included results of the functions which may be called in this function.
}

\author{Andre Duesterhus}
\seealso{\code{\link{qat_save_lim_rule_static_1d}}, \code{\link{qat_save_lim_rule_sigma_1d}}, \code{\link{qat_save_lim_rule_dynamic_1d}}, \code{\link{qat_run_workflow_save}}}

\examples{
	vec <- rnorm(1000)
	min_vector<-seq(-1,-2,length.out=1000)
	max_vector<-seq(1,2,length.out=1000)
	workflowlist_part <- list(minimum_value=-2, maximum_value=2,minimum_vector="vec1",maximum_vector="vec2",minimum_vector_name="minimum vector",maximum_vector_name="maximum vector", sigma_factor=2)
	resultlist <- qat_call_lim_rule(vec, workflowlist_part, element=1, vec1=min_vector, vec2=max_vector)
	savelist <- list()
	savelistcounter <- 1
	for (ii in 2:4) {
		savelist <- qat_call_save_lim_rule(resultlist[[ii]], savelist=savelist, savelistcounter=savelistcounter)
		if (length(which(names(savelist)=="element"))==0) {
			savelistcounter<-length(savelist)
		} else {
			savelistcounter<-1		
		}
	}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}