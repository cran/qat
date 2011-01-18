qat_save_result_ncdf <-
function(measurement_vector, savelist, filename, workflowlist=NULL, time=NULL, height= NULL, lat=NULL, lon=NULL, vec1=NULL,vec2=NULL,vec3=NULL,vec4=NULL, store_mes_vec=TRUE, baseunit="unitless", addunits = c("minutes","metres", "degrees", "degrees", "unitless", "unitless", "unitless", "unitless"), directoryname="", nan_value=-999, variable_name="", transformationonvariable="", authorname="", original_filename="", data_level="", workflow_filename="") {
	if (!is.null(savelist)&&(length(savelist)>0)){
		library("ncdf")
		# actually only 1D measurement-vectors are implemented
		if ((length(measurement_vector)>0)) {
			# store all vectors
			len_mes_vec <- length(measurement_vector)
			contvar_count<- 0
			contvar <- NULL
			variables <- NULL
			if (!is.null(workflowlist)) {
				workflowlist <- qat_add_all_algorithms(workflowlist)
				workflowlist <- qat_add_all_descriptions(workflowlist)
			}
			dim_mes_vec <- dim.def.ncdf("measurement_vector_index", "unitless", 1:len_mes_vec)
			if (store_mes_vec) {
				contvar[[contvar_count <- contvar_count+1]] <- measurement_vector
				var_temp <- var.def.ncdf(paste(variable_name,"measurement_vector", sep=""), baseunit, dim_mes_vec, -999, longname="Measurement Vector")
				variables[[contvar_count]] <- var_temp
			} else {
	#			variables <- NULL
	#			contvar <- NULL
			}
			# time vector
			if (!is.null(time)) {
				if (sum(is.nan(time) > 0)) {
					time[is.nan(time)] <- nan_value
				}
				if (length(time) != len_mes_vec) {
					dim_time <- dim.def.ncdf("time", addunits[1], time)
					var_temp <- var.def.ncdf("timevec", addunits[1], dim_time, nan_value, longname="Time Vector")
				} else {
					var_temp <- var.def.ncdf("timevec", addunits[1], dim_mes_vec, nan_value, longname="Time Vector")
				}
				contvar[[contvar_count <- contvar_count+1]] <- time
				variables[[contvar_count]] <- var_temp
			}
			if (!is.null(height)) {
				if (sum(is.nan(height) > 0)) {
					lat[is.nan(height)] <- nan_value
				}
				if (length(lat) != len_mes_vec) {
					dim_height <- dim.def.ncdf("height", addunits[2], height)
					var_temp <- var.def.ncdf("heightvec", addunits[2], dim_height, nan_value, longname="Height Vector")
				} else {
					var_temp <- var.def.ncdf("heightvec", addunits[2], dim_mes_vec, nan_value, longname="Height Vector")
				}
				contvar[[contvar_count <- contvar_count+1]] <- height
				variables[[contvar_count]] <- var_temp	
			}
			if (!is.null(lat)) {
				if (sum(is.nan(lat) > 0)) {
					lat[is.nan(lat)] <- nan_value
				}
				if (length(lat) != len_mes_vec) {
					dim_lat <- dim.def.ncdf("latitude", addunits[3], lat)
					var_temp <- var.def.ncdf("latitudevec", addunits[3], dim_lat, nan_value, longname="Latitude Vector")
				} else {
					var_temp <- var.def.ncdf("latitudevec", addunits[3], dim_mes_vec, nan_value, longname="Latitude Vector")
				}
				contvar[[contvar_count <- contvar_count+1]] <- lat
				variables[[contvar_count]] <- var_temp	
			}
			if (!is.null(lon)) {
				if (sum(is.nan(lon) > 0)) {
					lat[is.nan(lon)] <- nan_value
				}
				if (length(lat) != len_mes_vec) {
					dim_lon <- dim.def.ncdf("longitude", addunits[4], lon)
					var_temp <- var.def.ncdf("longitudevec", addunits[4], dim_lon, nan_value, longname="Longitude Vector")
				} else {
					var_temp <- var.def.ncdf("longitudevec", addunits[4], dim_mes_vec, nan_value, longname="Longitude Vector")
				}
				contvar[[contvar_count <- contvar_count+1]] <- lon
				variables[[contvar_count]] <- var_temp	
			}
			if (!is.null(vec1)) {
				if (sum(is.nan(vec1) > 0)) {
					lat[is.nan(vec1)] <- nan_value
				}
				if (length(vec1) != len_mes_vec) {
					dim_vec1 <- dim.def.ncdf("addvec1_index", addunits[5], 1:length(vec1))
					var_temp <- var.def.ncdf("addvec1", addunits[5], dim_vec1, nan_value, longname="Additional Vector 1")
				} else {
					var_temp <- var.def.ncdf("addvec1", addunits[5], dim_mes_vec, nan_value, longname="Additional Vector 1")
				}
				contvar[[contvar_count <- contvar_count+1]] <- vec1
				variables[[contvar_count]] <- var_temp	
			}
			if (!is.null(vec2)) {
				if (sum(is.nan(vec2) > 0)) {
					lat[is.nan(vec2)] <- nan_value
				}
				if (length(vec2) != len_mes_vec) {
					dim_vec2 <- dim.def.ncdf("addvec2_index", addunits[6], 1:length(vec2))
					var_temp <- var.def.ncdf("addvec2", addunits[6], dim_vec2, nan_value, longname="Additional Vector 2")
				} else {
					var_temp <- var.def.ncdf("addvec2", addunits[6], dim_mes_vec, nan_value, longname="Additional Vector 2")
				}
				contvar[[contvar_count <- contvar_count+1]] <- vec2
				variables[[contvar_count]] <- var_temp	
			}
			if (!is.null(vec3)) {
				if (sum(is.nan(vec3) > 0)) {
					lat[is.nan(vec3)] <- nan_value
				}
				if (length(vec3) != len_mes_vec) {
					dim_vec3 <- dim.def.ncdf("addvec3_index", addunits[7], 1:length(vec3))
					var_temp <- var.def.ncdf("addvec3", addunits[7], dim_vec3, nan_value, longname="Additional Vector 3")
				} else {
					var_temp <- var.def.ncdf("addvec3", addunits[7], dim_mes_vec, nan_value, longname="Additional Vector 3")
				}
				contvar[[contvar_count <- contvar_count+1]] <- vec3
				variables[[contvar_count]] <- var_temp	
			}
			if (!is.null(vec4)) {
				if (sum(is.nan(vec4) > 0)) {
					lat[is.nan(vec4)] <- nan_value
				}
				if (length(vec4) != len_mes_vec) {
					dim_vec4 <- dim.def.ncdf("addvec4_index", addunits[8], 1:length(vec4))
					var_temp <- var.def.ncdf("addvec4", addunits[8], dim_vec4, nan_value, longname="Additional Vector 4")
				} else {
					var_temp <- var.def.ncdf("addvec4", addunits[8], dim_mes_vec, nan_value, longname="Additional Vector 4")
				}
				contvar[[contvar_count <- contvar_count+1]] <- vec4
				variables[[contvar_count]] <- var_temp	
			}
			contvar_count_secure <- contvar_count
			# constructing variables and dimensions
			for (ii in 2:length(savelist)) {
				if (is.null(savelist[[ii]]$tosave$returntext)) {
					for(jj in 1:length(savelist[[ii]]$tosave$dimension)) {
						if (names(savelist[[ii]]$tosave$dimension)[jj]=="mes_vec") {
							var_temp <- var.def.ncdf(paste(variable_name,"_",savelist[[ii]]$tosave$method,"_", savelist[[ii]]$element,"_" , names(savelist[[ii]]$tosave$longname)[jj], sep=""), savelist[[ii]]$tosave$unit[[jj]], dim_mes_vec, savelist[[ii]]$tosave$fillvalue, longname=savelist[[ii]]$tosave$longname[[jj]])
						} else {
							dim_temp <- dim.def.ncdf(names(savelist[[ii]]$tosave$dimension)[jj], savelist[[ii]]$tosave$unit[[jj]], 1:savelist[[ii]]$tosave$dimension[[jj]])
							var_temp <- var.def.ncdf(paste(variable_name,"_",savelist[[ii]]$tosave$method,"_", savelist[[ii]]$element,"_" , names(savelist[[ii]]$tosave$longname)[jj], sep=""), savelist[[ii]]$tosave$unit[[jj]], dim_temp, savelist[[ii]]$tosave$fillvalue, longname=savelist[[ii]]$tosave$longname[[jj]])
						}
						if (!is.null(savelist[[ii]]$tosave$content[[jj]])) {
							contvar[[contvar_count <- contvar_count+1]] <- savelist[[ii]]$tosave$content[[jj]]
						} else {
							if (names(savelist[[ii]]$tosave$dimension)[jj]=="mes_vec") {
								contvar[[contvar_count <- contvar_count+1]] <- rep(NaN,len_mes_vec)
							} else {
								contvar[[contvar_count <- contvar_count+1]] <- rep(NaN,savelist[[ii]]$tosave$dimension[[jj]])
							}
						}
						variables[[contvar_count]] <- var_temp	
					}
				}
			}
			ncnew <- create.ncdf(paste(directoryname,filename,".nc", sep=""),variables, verbose=FALSE)
			contvar_count <- contvar_count_secure
			for (ii in 2:length(savelist)) {
				if (is.null(savelist[[ii]]$tosave$returntext)) {
					for(jj in 1:length(savelist[[ii]]$tosave$dimension)) {
						contvar_count <- contvar_count + 1
						if (!is.null(savelist[[ii]]$tosave$meanings)) {
							att.put.ncdf(ncnew, variables[[contvar_count]], "meanings", savelist[[ii]]$tosave$meanings[[jj]])
						}
						if (!is.null(savelist[[ii]]$tosave$unit)) {
							att.put.ncdf(ncnew, variables[[contvar_count]], "unit", savelist[[ii]]$tosave$unit[[jj]])
						}
						if (!is.null(workflowlist[[savelist[[ii]]$element]]$additional_information$description)) {
							att.put.ncdf(ncnew, variables[[contvar_count]], "description", workflowlist[[savelist[[ii]]$element]]$additional_information$description)
						}
						if (!is.null(workflowlist[[savelist[[ii]]$element]]$additional_information$algorithm)) {
							att.put.ncdf(ncnew, variables[[contvar_count]], "algorithm", workflowlist[[savelist[[ii]]$element]]$additional_information$algorithm)
						}
						if (!is.null(workflowlist[[savelist[[ii]]$element]]$additional_information$result$comment_on_result)) {
							att.put.ncdf(ncnew, variables[[contvar_count]], "comment", workflowlist[[savelist[[ii]]$element]]$additional_information$result$comment_on_result)
						}
						if (transformationonvariable != "") {
							att.put.ncdf(ncnew, variables[[contvar_count]], "testedvariable_transformation", transformationonvariable)
						}
						if (data_level != "") {
							att.put.ncdf(ncnew, variables[[contvar_count]], "data_level", data_level)
						}
						if (!is.null(savelist[[ii]]$tosave$parameter)) {
							for (kk in 1:length(savelist[[ii]]$tosave$parameter)) {
								if (!is.null(savelist[[ii]]$tosave$parameter[[kk]])) {
									att.put.ncdf(ncnew, variables[[contvar_count]], paste("parameter_", names(savelist[[ii]]$tosave$parameter)[kk], sep=""), savelist[[ii]]$tosave$parameter[[kk]])
								}
							}
						}			
					}
				} else {
					if (transformationonvariable != "") {
						transformationonvariable <- paste(transformationonvariable, ":", savelist[[ii]]$tosave$returntext)
					} else {
						transformationonvariable <- savelist[[ii]]$tosave$returntext
					}
				}
			}
			if (authorname != "") {
				att.put.ncdf(ncnew, 0, "author", authorname)
			}
#			att.put.ncdf(ncnew, 0, "date", date())
			att.put.ncdf(ncnew, 0, "title", "Quality Assurance Information")
			if ((variable_name != "") || (original_filename != "")) {
				comm <- "This file includes quality information"
				if (variable_name != "") {
					comm <- paste(comm, "on the variable",variable_name)
				}
				if (original_filename != "") {
					comm <- paste(comm, "of the file",original_filename)
				}
				att.put.ncdf(ncnew, 0, "comment", comm)
			}
			sour <- "quality information"
			if (original_filename!="") {
				sour <- paste(sour, ":", original_filename)
			}
			if (workflow_filename!="") {
				sour <- paste(sour, ":", workflow_filename)
			}
			att.put.ncdf(ncnew, 0, "source", sour)
			att.put.ncdf(ncnew, 0, "reference", "This file was produced with help of the R-Package qat, version 0.1")
			att.put.ncdf(ncnew, 0, "history", paste(date(),"Performance of test"))

			for (ii in 1:contvar_count) {
				vari <- variables[[ii]]
				conti <- contvar[[ii]]
				conti[is.nan(conti)] <- nan_value
				put.var.ncdf(ncnew, vari$name, conti)
			}
			close.ncdf(ncnew)
		}
	}
}
