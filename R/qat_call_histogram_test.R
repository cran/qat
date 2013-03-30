qat_call_histogram_test <-
function(measurement_vector, workflowlist_part, element=-999, time=NULL, height= NULL, lat=NULL, lon=NULL, vec1=NULL,vec2=NULL,vec3=NULL,vec4=NULL,resultlist=list(), resultlistcounter=1) {
## functionality: calling function for qat_analyse_histogram_test_kld_1d, qat_analyse_histogram_test_jsd_1d, qat_analyse_histogram_test_rms_1d, qat_analyse_histogram_test_ms_1d, qat_analyse_histogram_test_emd_1d
## author: André Düsterhus
## date: 09.03.2010
## version: A0.1
## input: measurement_vector, workflowlist element, number of actual element, time vector (optional), latitude vector (optional), longitude vector (optional), 4 optional vectors, resultlist (optional), counter of resultlist (optional)
## output: list with the results and parameters of the lim rule analysis
	# check if method is given
	if(!is.null(workflowlist_part$metric)) {
		if (tolower(workflowlist_part$metric) == 'kld') {
			# control of parameters. function handles this with default when they are not given and therefore set to NULL
			if(!is.null(workflowlist_part$blocksize)) {
				blocksize <- as.numeric(workflowlist_part$blocksize)
			} else {
				blocksize <- NULL
			}
			if(!is.null(workflowlist_part$numofbars)) {
				numofbars <-  as.numeric(workflowlist_part$numofbars)
			} else {
				numofbars <- NULL
			}
			if(!is.null(workflowlist_part$factorofbar)) {
				factorofbar <-  as.numeric(workflowlist_part$factorofbar)
			} else {
				factorofbar <- NULL
			}
			# call of function
			if (is.null(dim(measurement_vector))) {
				resultlist[[resultlistcounter <- resultlistcounter+1]] <- list(element=element, method='histtest_kld', result =qat_analyse_histogram_test_kld_1d(measurement_vector, blocksize, numofbars, factorofbar))
			}
			if (length(dim(measurement_vector))==2) {
				resultlist[[resultlistcounter <- resultlistcounter+1]] <- list(element=element, method='histtest_kld', result =qat_analyse_histogram_test_kld_2d(measurement_vector, blocksize, numofbars, factorofbar))				
			}		
		}
		if (tolower(workflowlist_part$metric) == 'jsd') {
			# control of parameters. function handles this with default when they are not given and therefore set to NULL
			if(!is.null(workflowlist_part$blocksize)) {
				blocksize <-  as.numeric(workflowlist_part$blocksize)
			} else {
				blocksize <- NULL
			}
			if(!is.null(workflowlist_part$numofbars)) {
				numofbars <-  as.numeric(workflowlist_part$numofbars)
			} else {
				numofbars <- NULL
			}
			if(!is.null(workflowlist_part$factorofbar)) {
				factorofbar <-  as.numeric(workflowlist_part$factorofbar)
			} else {
				factorofbar <- NULL
			}
			# call of function
			if (is.null(dim(measurement_vector))) {
				resultlist[[resultlistcounter <- resultlistcounter+1]] <- list(element=element, method='histtest_jsd', result =qat_analyse_histogram_test_jsd_1d(measurement_vector, blocksize, numofbars, factorofbar))
			}
			if (length(dim(measurement_vector))==2) {
				resultlist[[resultlistcounter <- resultlistcounter+1]] <- list(element=element, method='histtest_jsd', result =qat_analyse_histogram_test_jsd_2d(measurement_vector, blocksize, numofbars, factorofbar))				
			}		
		}
		if (tolower(workflowlist_part$metric) == 'rms') {
			# control of parameters. function handles this with default when they are not given and therefore set to NULL
			if(!is.null(workflowlist_part$blocksize)) {
				blocksize <-  as.numeric(workflowlist_part$blocksize)
			} else {
				blocksize <- NULL
			}
			if(!is.null(workflowlist_part$numofbars)) {
				numofbars <-  as.numeric(workflowlist_part$numofbars)
			} else {
				numofbars <- NULL
			}
			# call of function
			if (is.null(dim(measurement_vector))) {
				resultlist[[resultlistcounter <- resultlistcounter+1]] <- list(element=element, method='histtest_rms', result =qat_analyse_histogram_test_rms_1d(measurement_vector, blocksize, numofbars))
			}
			if (length(dim(measurement_vector))==2) {
				resultlist[[resultlistcounter <- resultlistcounter+1]] <- list(element=element, method='histtest_rms', result =qat_analyse_histogram_test_rms_2d(measurement_vector, blocksize, numofbars))				
			}		
		}
		if (tolower(workflowlist_part$metric) == 'ms') {
			# control of parameters. function handles this with default when they are not given and therefore set to NULL
			if(!is.null(workflowlist_part$blocksize)) {
				blocksize <-  as.numeric(workflowlist_part$blocksize)
			} else {
				blocksize <- NULL
			}
			if(!is.null(workflowlist_part$numofbars)) {
				numofbars <-  as.numeric(workflowlist_part$numofbars)
			} else {
				numofbars <- NULL
			}
			# call of function
			if (is.null(dim(measurement_vector))) {
				resultlist[[resultlistcounter <- resultlistcounter+1]] <- list(element=element, method='histtest_ms', result =qat_analyse_histogram_test_ms_1d(measurement_vector, blocksize, numofbars))
			}
			if (length(dim(measurement_vector))==2) {
				resultlist[[resultlistcounter <- resultlistcounter+1]] <- list(element=element, method='histtest_ms', result =qat_analyse_histogram_test_ms_2d(measurement_vector, blocksize, numofbars))				
			}		
		}
		if (tolower(workflowlist_part$metric) == 'emd') {
			# control of parameters. function handles this with default when they are not given and therefore set to NULL
			if(!is.null(workflowlist_part$blocksize)) {
				blocksize <-  as.numeric(workflowlist_part$blocksize)
			} else {
				blocksize <- NULL
			}
			if(!is.null(workflowlist_part$numofbars)) {
				numofbars <-  as.numeric(workflowlist_part$numofbars)
			} else {
				numofbars <- NULL
			}
			# call of function
			if (is.null(dim(measurement_vector))) {
				resultlist[[resultlistcounter <- resultlistcounter+1]] <- list(element=element, method='histtest_emd', result =qat_analyse_histogram_test_emd_1d(measurement_vector, blocksize, numofbars))
			}
			if (length(dim(measurement_vector))==2) {
				resultlist[[resultlistcounter <- resultlistcounter+1]] <- list(element=element, method='histtest_emd', result =qat_analyse_histogram_test_emd_2d(measurement_vector, blocksize, numofbars))
			}		
		}
	}
	return(resultlist)
}
