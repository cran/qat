qat_save_result_ncdf <-
function(measurement_vector, savelist, filename, workflowlist=NULL, time=NULL, height= NULL, lat=NULL, lon=NULL, vec1=NULL,vec2=NULL,vec3=NULL,vec4=NULL, store_mes_vec=TRUE, baseunit="unitless", addunits = c("minutes","metres", "degrees", "degrees", "unitless", "unitless", "unitless", "unitless"), directoryname="", nan_value=-999, variable_name="", transformationonvariable="", authorname="", original_filename="", data_level="", workflow_filename="") {
## functionality: save 
## author: André Düsterhus
## date: 19.10.2012
## version: A0.1
## input: measurement vector, savelist, filename, optional: workflowlist, time vector, latitude vector, longitude vector, additional vectors, bool wheather store the measurement vector, baseunit, units of the dimensions, name of a directory, missing value, variable name, information on transformation on variable, name of the author, original filename, data level, workflow filename
## output: a netcdf file stored at filename with the results
	if (!is.null(savelist)&&(length(savelist)>0)){
#		library("ncdf")
		if (is.null(dim(measurement_vector))) {
			dim_mv <- 1
		} else {
			dim_mv <- length(dim(measurement_vector))
		}		
		contvar_count<- 0
		contvar <- NULL
		variables <- NULL
		# enrich workflow with internal information
		if (!is.null(workflowlist)) {
			workflowlist <- qat_add_all_algorithms(workflowlist)
			workflowlist <- qat_add_all_descriptions(workflowlist)
		}
		# define dimension(s) for mv		
		if (dim_mv == 1) {
			len_mes_vec <- length(measurement_vector)
			dim_mes_vec <- dim.def.ncdf("measurement_vector_index", "unitless", 1:length(measurement_vector))
		} else {
			dim_mes_vec <- list()
			for (ii in 1:dim_mv) {
				dim_mes_vec[[ii]] <- dim.def.ncdf(paste("measurement_vector_index",ii,sep=""), "unitless", 1:dim(measurement_vector)[ii])
			}
		}
		# storing mv if asked to do so
		if (store_mes_vec) {
			contvar[[contvar_count <- contvar_count+1]] <- measurement_vector
			var_temp <- var.def.ncdf(paste(variable_name,"measurement_vector", sep=""), baseunit, dim_mes_vec, -999, longname="Measurement Vector")
			variables[[contvar_count]] <- var_temp
		} else {
#			variables <- NULL
#			contvar <- NULL
		}
		if (!is.null(time)) {
			if (sum(is.nan(time) > 0)) {
				time[is.nan(time)] <- nan_value
			}
			if (is.null(dim(time))) {
				if (length(time) != len_mes_vec) {
					dim_time <- dim.def.ncdf("time", addunits[1], time)
					var_temp <- var.def.ncdf("timevec", addunits[1], dim_time, nan_value, longname="Time Vector")
				} else {
					var_temp <- var.def.ncdf("timevec", addunits[1], dim_mes_vec, nan_value, longname="Time Vector")
				}
			} else {
				if (sum(dim(measurement_vector)==dim(time))==length(dim(measurement_vector))) {
					var_temp <- var.def.ncdf("timevec", addunits[1], dim_mes_vec, nan_value, longname="Time Vector")
				} else {
					for (ii in 1:length(dim(time))) {
						dim_time <- list()
						dim_time[[ii]] <- dim.def.ncdf(paste("time",ii,sep=""), addunits[1], 1:dim(time)[ii])
					}
					var_temp <- var.def.ncdf("timevec", addunits[1], dim_time, nan_value, longname="Time Vector")
				}
			}
			contvar[[contvar_count <- contvar_count+1]] <- time
			variables[[contvar_count]] <- var_temp
		}
		if (!is.null(height)) {
			if (sum(is.nan(height) > 0)) {
				time[is.nan(height)] <- nan_value
			}
			if (is.null(dim(height))) {
				if (length(height) != len_mes_vec) {
					dim_height <- dim.def.ncdf("height", addunits[2], height)
					var_temp <- var.def.ncdf("heightvec", addunits[2], dim_height, nan_value, longname="Height Vector")
				} else {
					var_temp <- var.def.ncdf("heightvec", addunits[2], dim_mes_vec, nan_value, longname="Height Vector")
				}
			} else {
				if (sum(dim(measurement_vector)==dim(height))==length(dim(measurement_vector))) {
					var_temp <- var.def.ncdf("heightvec", addunits[2], dim_mes_vec, nan_value, longname="Height Vector")
				} else {
					for (ii in 1:length(dim(height))) {
						dim_height <- list()
						dim_height[[ii]] <- dim.def.ncdf(paste("height",ii,sep=""), addunits[2], 1:dim(height)[ii])
					}
					var_temp <- var.def.ncdf("heightvec", addunits[2], dim_height, nan_value, longname="Height Vector")
				}
			}
			contvar[[contvar_count <- contvar_count+1]] <- height
			variables[[contvar_count]] <- var_temp
		}
		if (!is.null(lat)) {
			if (sum(is.nan(lat) > 0)) {
				time[is.nan(lat)] <- nan_value
			}
			if (is.null(dim(lat))) {
				if (length(lat) != len_mes_vec) {
					dim_lat <- dim.def.ncdf("latitude", addunits[3], lat)
					var_temp <- var.def.ncdf("latitudevec", addunits[3], dim_lat, nan_value, longname="Latitude Vector")
				} else {
					var_temp <- var.def.ncdf("latitudevec", addunits[3], dim_mes_vec, nan_value, longname="Latitude Vector")
				}
			} else {
				if (sum(dim(measurement_vector)==dim(lat))==length(dim(measurement_vector))) {
					var_temp <- var.def.ncdf("latitudevec", addunits[3], dim_mes_vec, nan_value, longname="Latitude Vector")
				} else {
					for (ii in 1:length(dim(lat))) {
						dim_lat <- list()
						dim_lat[[ii]] <- dim.def.ncdf(paste("latitude",ii,sep=""), addunits[3], 1:dim(lat)[ii])
					}
					var_temp <- var.def.ncdf("latitudevec", addunits[3], dim_lat, nan_value, longname="Latitude Vector")
				}
			}
			contvar[[contvar_count <- contvar_count+1]] <- lat
			variables[[contvar_count]] <- var_temp
		}
		if (!is.null(lon)) {
			if (sum(is.nan(lon) > 0)) {
				time[is.nan(lon)] <- nan_value
			}
			if (is.null(dim(lon))) {
				if (length(lon) != len_mes_vec) {
					dim_lon <- dim.def.ncdf("longitude", addunits[4], lon)
					var_temp <- var.def.ncdf("longitudevec", addunits[4], dim_lon, nan_value, longname="Longitude Vector")
				} else {
					var_temp <- var.def.ncdf("longitudevec", addunits[4], dim_mes_vec, nan_value, longname="Longitude Vector")
				}
			} else {
				if (sum(dim(measurement_vector)==dim(lon))==length(dim(measurement_vector))) {
					var_temp <- var.def.ncdf("longitudevec", addunits[4], dim_mes_vec, nan_value, longname="Longitude Vector")
				} else {
					for (ii in 1:length(dim(lon))) {
						dim_lon <- list()
						dim_lon[[ii]] <- dim.def.ncdf(paste("longitude",ii,sep=""), addunits[4], 1:dim(lon)[ii])
					}
					var_temp <- var.def.ncdf("longitudevec", addunits[4], dim_lon, nan_value, longname="Longitude Vector")
				}
			}
			contvar[[contvar_count <- contvar_count+1]] <- lon
			variables[[contvar_count]] <- var_temp	
		}
		if (!is.null(vec1)) {
			if (sum(is.nan(vec1) > 0)) {
				time[is.nan(vec1)] <- nan_value
			}
			if (is.null(dim(vec1))) {
				if (length(vec1) != len_mes_vec) {
					dim_vec1 <- dim.def.ncdf("addvec1_index", addunits[5], 1:length(vec1))
					var_temp <- var.def.ncdf("addvec1", addunits[5], dim_vec1, nan_value, longname="Additional Vector 1")
				} else {
					var_temp <- var.def.ncdf("addvec1", addunits[5], dim_mes_vec, nan_value, longname="Additional Vector 1")
				}
			} else {
				if (sum(dim(measurement_vector)==dim(vec1))==length(dim(measurement_vector))) {
					var_temp <- var.def.ncdf("addvec1", addunits[5], dim_mes_vec, nan_value, longname="Additional Vector 1")
				} else {
					for (ii in 1:length(dim(vec1))) {
						dim_vec1 <- list()
						dim_vec1[[ii]] <- dim.def.ncdf(paste("addvec1_index",ii,sep=""), addunits[5], 1:dim(vec1)[ii])
					}
					var_temp <- var.def.ncdf("addvec1", addunits[5], dim_vec1, nan_value, longname="Additional Vector 1")
				}
			}
			contvar[[contvar_count <- contvar_count+1]] <- vec1
			variables[[contvar_count]] <- var_temp				
		}
		if (!is.null(vec2)) {
			if (sum(is.nan(vec2) > 0)) {
				time[is.nan(vec2)] <- nan_value
			}
			if (is.null(dim(vec2))) {
				if (length(vec2) != len_mes_vec) {
					dim_vec2 <- dim.def.ncdf("addvec2_index", addunits[6], 1:length(vec2))
					var_temp <- var.def.ncdf("addvec2", addunits[6], dim_vec2, nan_value, longname="Additional Vector 2")
				} else {
					var_temp <- var.def.ncdf("addvec2", addunits[6], dim_mes_vec, nan_value, longname="Additional Vector 2")
				}
			} else {
				if (sum(dim(measurement_vector)==dim(vec2))==length(dim(measurement_vector))) {
					var_temp <- var.def.ncdf("addvec2", addunits[6], dim_mes_vec, nan_value, longname="Additional Vector 2")
				} else {
					for (ii in 1:length(dim(vec2))) {
						dim_vec2 <- list()
						dim_vec2[[ii]] <- dim.def.ncdf(paste("addvec2_index",ii,sep=""), addunits[6], 1:dim(vec2)[ii])
					}
					var_temp <- var.def.ncdf("addvec2", addunits[6], dim_vec2, nan_value, longname="Additional Vector 2")
				}
			}
			contvar[[contvar_count <- contvar_count+1]] <- vec2
			variables[[contvar_count]] <- var_temp			
		}
		if (!is.null(vec3)) {
			if (sum(is.nan(vec3) > 0)) {
				time[is.nan(vec3)] <- nan_value
			}
			if (is.null(dim(vec3))) {
				if (length(vec3) != len_mes_vec) {
					dim_vec3 <- dim.def.ncdf("addvec3_index", addunits[7], 1:length(vec3))
					var_temp <- var.def.ncdf("addvec3", addunits[7], dim_vec3, nan_value, longname="Additional Vector 3")
				} else {
					var_temp <- var.def.ncdf("addvec3", addunits[7], dim_mes_vec, nan_value, longname="Additional Vector 3")
				}
			} else {
				if (sum(dim(measurement_vector)==dim(vec3))==length(dim(measurement_vector))) {
					var_temp <- var.def.ncdf("addvec3", addunits[7], dim_mes_vec, nan_value, longname="Additional Vector 3")
				} else {
					for (ii in 1:length(dim(vec3))) {
						dim_vec3 <- list()
						dim_vec3[[ii]] <- dim.def.ncdf(paste("addvec3_index",ii,sep=""), addunits[7], 1:dim(vec3)[ii])
					}
					var_temp <- var.def.ncdf("addvec3", addunits[7], dim_vec3, nan_value, longname="Additional Vector 3")
				}
			}
			contvar[[contvar_count <- contvar_count+1]] <- vec3
			variables[[contvar_count]] <- var_temp				
		}
		if (!is.null(vec4)) {
			if (sum(is.nan(vec4) > 0)) {
				time[is.nan(vec4)] <- nan_value
			}
			if (is.null(dim(vec4))) {
				if (length(vec4) != len_mes_vec) {
					dim_vec4 <- dim.def.ncdf("addvec4_index", addunits[8], 1:length(vec4))
					var_temp <- var.def.ncdf("addvec4", addunits[8], dim_vec4, nan_value, longname="Additional Vector 4")
				} else {
					var_temp <- var.def.ncdf("addvec4", addunits[8], dim_mes_vec, nan_value, longname="Additional Vector 4")
				}
			} else {
				if (sum(dim(measurement_vector)==dim(vec4))==length(dim(measurement_vector))) {
					var_temp <- var.def.ncdf("addvec4", addunits[8], dim_mes_vec, nan_value, longname="Additional Vector 4")
				} else {
					for (ii in 1:length(dim(vec4))) {
						dim_vec4 <- list()
						dim_vec4[[ii]] <- dim.def.ncdf(paste("addvec4_index",ii,sep=""), addunits[8], 1:dim(vec4)[ii])
					}
					var_temp <- var.def.ncdf("addvec4", addunits[8], dim_vec4, nan_value, longname="Additional Vector 4")
				}
			}
			contvar[[contvar_count <- contvar_count+1]] <- vec4
			variables[[contvar_count]] <- var_temp				
		}
		contvar_count_secure <- contvar_count
		# constructing variables and dimensions
		for (ii in 2:length(savelist)) {
			if (is.null(savelist[[ii]]$tosave$returntext)) {
#				print((savelist[[ii]]$tosave$dimension))
				for(jj in 1:length(savelist[[ii]]$tosave$dimension)) {
					if (is.list(savelist[[ii]]$tosave$dimension[[jj]])) {
						dim_temp <- list()
						for (kk in 1:length(savelist[[ii]]$tosave$dimension[[jj]])) {
#							print(kk)
							# ask if it is a dimension of a measurement vector
							if (substr(names(savelist[[ii]]$tosave$dimension[[jj]])[kk], 1, 7)=="mes_vec") {
								if (nchar(names(savelist[[ii]]$tosave$dimension[[jj]])[kk]) >= 8) {
									mesvec_num <- as.integer(substr(names(savelist[[ii]]$tosave$dimension[[jj]])[kk], 8, nchar(names(savelist[[ii]]$tosave$dimension[[jj]])[kk])))
#									if(kk==1){
#									print(names(savelist[[ii]]$tosave$dimension[[jj]])[kk])
#									print(nchar(names(savelist[[ii]]$tosave$dimension[[jj]])[kk]))
#									}
									dim_temp[[kk]] <- dim_mes_vec[[mesvec_num]]
#									if(ii==12){
#									print(mesvec_num)
#									print(length(dim_temp))
#									}
									
								} else {
									dim_temp[[kk]] <- dim_mes_vec
								}
							} else {
								if (is.character(savelist[[ii]]$tosave$dimunit[[jj]][[kk]])) {
									dim_temp[[kk]] <- dim.def.ncdf(names(savelist[[ii]]$tosave$dimension[[jj]])[kk], savelist[[ii]]$tosave$dimunit[[jj]][[kk]], 1:savelist[[ii]]$tosave$dimension[[jj]][[kk]])
								} else {
									dim_temp[[kk]] <- dim.def.ncdf(names(savelist[[ii]]$tosave$dimension[[jj]])[kk], "unitless", 1:savelist[[ii]]$tosave$dimension[[jj]][[kk]])
								}
							}
						}
					} else {
						dim_temp <- list()
						if (substr(names(savelist[[ii]]$tosave$dimension)[jj], 1, 7)=="mes_vec") {
							if (length(savelist[[ii]]$tosave$dimension[[jj]]) >= 8) {
								mesvec_num <- substr(savelist[[ii]]$tosave$dimension[[jj]], 7, length(savelist[[ii]]$tosave$dimension[[jj]]))
								dim_temp[[1]] <- dim_mes_vec[[mesvec_num]]
							} else {
								dim_temp[[1]] <- dim_mes_vec
							}
						} else {
							if (is.character(savelist[[ii]]$tosave$dimunit[[jj]])) {
								dim_temp[[1]] <- dim.def.ncdf(names(savelist[[ii]]$tosave$dimension)[jj], savelist[[ii]]$tosave$dimunit[[jj]], 1:savelist[[ii]]$tosave$dimension[[jj]])
							} else {
								dim_temp[[1]] <- dim.def.ncdf(names(savelist[[ii]]$tosave$dimension)[jj], "unitless", 1:savelist[[ii]]$tosave$dimension[[jj]])
							}
						}
					}
					var_temp <- var.def.ncdf(paste(variable_name,"_",savelist[[ii]]$tosave$method,"_", savelist[[ii]]$element,"_" , names(savelist[[ii]]$tosave$longname)[jj], sep=""), savelist[[ii]]$tosave$unit[[jj]], dim_temp, savelist[[ii]]$tosave$fillvalue, longname=savelist[[ii]]$tosave$longname[[jj]])
#					if (((ii==12)&&(jj==1))||((ii==3)&&(jj==1))) {
#						print("test")
#						print(var_temp)
#					}
					# store variables
					if (!is.null(savelist[[ii]]$tosave$content[[jj]])) {
						contvar[[contvar_count <- contvar_count+1]] <- savelist[[ii]]$tosave$content[[jj]]
					} else {
						contvar[[contvar_count <- contvar_count+1]] <- NaN
					}
					variables[[contvar_count]] <- var_temp
				}
			}
		}
		# create file
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
		att.put.ncdf(ncnew, 0, "reference", "This file was produced with help of the R-Package qat, version 0.6")
		att.put.ncdf(ncnew, 0, "history", paste(date(),"Performance of test"))

		for (ii in 1:contvar_count) {
			vari <- variables[[ii]]
			conti <- contvar[[ii]]
			conti[is.nan(conti)] <- nan_value
#			if (ii==18) {
#				print(vari)
#				print(dim(conti))
#			}
			put.var.ncdf(ncnew, vari$name, conti)
		}
		close.ncdf(ncnew)
	}
}
