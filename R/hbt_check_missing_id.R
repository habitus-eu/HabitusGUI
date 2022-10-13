#' hbt_check_missing_id
#'
#' @param participant_basis object as loaded inside PALMSplusRshiny
#' @param palmsplus_folder output folder path for storing log of excluded IDs
#' @param dataset_name Name of dataset
#' @param palms palms object as loaded inside PALMSplusRshiny
#' @param loca Nested list with location information
#' @param groupinglocation groupinglocation
#' @return List with participant_basis and palms object without non-matching IDs
#'
#' @export
#' 
hbt_check_missing_id = function(participant_basis, palmsplus_folder, dataset_name, palms,
                                loca, groupinglocation = "school") {
  
  # Test for missing values in participant basis
  locationNames = names(loca)
  locationNames2 = NULL
  if (groupinglocation %in% locationNames) {
    locationNames2 = locationNames[which(locationNames == groupinglocation)] # focus here on school
  } else {
    locationNames2 = locationNames
  }
  loc_id = paste0(locationNames[which(names(participant_basis) %in% paste0(locationNames2, "_id"))][1], "_id")
  
  test_missing_value = rowSums(is.na(participant_basis[,c("identifier", loc_id)])) #"school_id"
  missing = which(test_missing_value > 1)
  participant_exclude_list = list(identifier = NULL, loc_id = NULL)
  names(participant_exclude_list)[2] = loc_id
  
  if (length(missing) > 0) {
    cat("\nMissing ID values in participant_basis\n")
    cat(paste0("  Ignoring identifier ", paste(participant_basis$identifier[missing], sep = " "), "\n"))
    # cat(paste0("  Ignoring schoolid ", paste(participant_basis$school_id[missing], sep = " "), "\n"))
    cat(paste0("  Ignoring ", loc_id, " ", paste(participant_basis[[loc_id]][missing], sep = " "), "\n"))
    participant_exclude_list$identifier = participant_basis$identifier[missing]
    # participant_exclude_list$school_id = participant_basis$school_id[missing]
    participant_exclude_list[[loc_id]] = participant_basis[[loc_id]][missing]
    participant_basis = participant_basis[test_missing_value == 0, ]
  }
  # Write list of excluded files to file
  sink(paste0(palmsplus_folder, "/", dataset_name, "_excluded_ids.txt"))
  print(participant_exclude_list)
  sink()
  rm(missing)
  
  # Test for incomplete id in palms object
  missing_identifiers = unique(c(palms$identifier[which(palms$identifier %in% participant_basis$identifier == FALSE)],
                                 participant_basis$identifier[which(participant_basis$identifier %in% palms$identifier == FALSE)]))

  if (length(missing_identifiers) > 0) {
    cat("\nRemoving missing identifiers related to palms: ")
    cat(missing_identifiers)
    cat("\n")
    participant_basis = participant_basis[participant_basis$identifier %in% missing_identifiers == FALSE,]
    palms = palms[palms$identifier %in% missing_identifiers == FALSE,]
  }
  
  # Check whether id is found in all objects
  check_N = function(loca, participant_basis, palms, groupinglocation) {
    # Check loca
    locationNames = names(loca)
    for (i in 1:length(loca)) {
      if (locationNames[i] != "home") {
        loc_id = paste0(groupinglocation, "_id")
      } else  if (locationNames[i] == "home") { # assumption that home is always the identifier for individuals
        loc_id = "identifier"
      } 
      for (j in 1:2) {
        N = length(unique(loca[[i]][j][[1]][[loc_id]]))
        tibblename = names(loca[[i]][j])
        if (N == 0) {
          cat(paste0("\nNo ",loc_id ," found in ", tibblename, "$", loc_id))
        } else {
          cat(paste0("\n  ", tibblename, ": ", N))
        }
      }
    }
    # OLD CODE:
    # if (length(unique(school$school_id)) == 0) {
    #   cat("\nNo school_id found in school$school_id")
    # }
    # if (length(unique(school_nbh$school_id)) == 0) {
    #   cat("\nNo school_id found in school_nbh$school_id")
    # }
    # if (length(unique(home$identifier)) == 0) {
    #   cat("\nNo identifier found in home$identifier")
    # }
    # if (length(unique(home_nbh$identifier)) == 0) {
    #   cat("\nNo identifier found in home_nbh$identifier")
    # }
    # if (length(unique(participant_basis$school_id)) == 0) {
    #   cat("\nNo school_id found in participant_basis$school_id")
    # }
    # Check participant_basis
    if (length(unique(participant_basis[[paste0(groupinglocation, "_id")]])) == 0) {
      cat(paste0("\nNo ", groupinglocation, "_id found in participant_basis$", groupinglocation, "_id"))
    }
    # NOT CHANGED:
    if (length(unique(participant_basis$identifier)) == 0) {
      cat("\nNo identifier found in participant_basis$identifier")
    }
    if (length(unique(palms$identifier)) == 0) {
      cat("\nNo identifier found in palms$identifier")
    }
    cat("\n")
  }

  # at this point we should have a cleaned dataset with only consistent data in all objects
  cat("\nNumber of unique IDs in all objects: ")
  cat(paste0("\n  Number of unique IDs in participant_basis: ", length(unique(participant_basis$identifier))))
  cat(paste0("\n  palms: ", length(unique(palms$identifier))))
  # OLD CODE: Now part of check_N function
  # cat(paste0("\n  home: ", length(unique(home$identifier))))
  # cat(paste0("\n  home_nbh: ", length(unique(home_nbh$identifier))))
  # cat(paste0("\n  school: ", length(unique(school$school_id))))
  # cat(paste0("\n  school_nbh: ", length(unique(school_nbh$school_id))))
  check_N(loca, participant_basis, palms, groupinglocation)
  cat("\n")
  
  # check_N(home, home_nbh, school_nbh, school_nbh, participant_basis, palms)
  
  # Test for incomplete shape files. I have commented this out as it is unclear whether 
  # missing shape files or redundant shape files is a problem for palmsplusr
  # home id
  # missing_identifiers = unique(c(home$identifier[which(home$identifier %in% participant_basis$identifier == FALSE)],
  #                         participant_basis$identifier[which(participant_basis$identifier %in% home$identifier == FALSE)],
  #                         home_nbh$identifier[which(home_nbh$identifier %in% participant_basis$identifier == FALSE)],
  #                         participant_basis$identifier[which(participant_basis$identifier %in% home_nbh$identifier == FALSE)]))
  # if (length(missing_identifiers) > 0) {
  #   print("Removing missing identifiers related to home")
  #   print(missing_identifiers)
  #   participant_basis = participant_basis[participant_basis$identifier %in% missing_identifiers == FALSE,]
  #   home = home[home$identifier %in% missing_identifiers == FALSE,]
  #   home_nbh = home_nbh[home_nbh$identifier %in% missing_identifiers == FALSE,]
  # }
  # check_N(home, home_nbh, school_nbh, school_nbh, participant_basis, palms)
  # 
  # missing_school_id = unique(c(school$school_id[which(school$school_id %in% participant_basis$school_id == FALSE)],
  #                                participant_basis$school_id[which(participant_basis$school_id %in% school$school_id == FALSE)],
  #                              school_nbh$school_id[which(school_nbh$school_id %in% participant_basis$school_id == FALSE)],
  #                              participant_basis$school_id[which(participant_basis$school_id %in% school_nbh$school_id == FALSE)]))
  # 
  # if (length( missing_school_id) > 0) {
  #   print("Removing missing schoolids")
  #   print(missing_school_id)
  #   participant_basis = participant_basis[participant_basis$school_id %in% missing_school_id == FALSE,]
  #   school = school[school$school_id %in%  missing_school_id == FALSE,]
  #   school_nbh = school_nbh[school_nbh$school_id %in% missing_school_id == FALSE,]
  # }
  # check_N(home, home_nbh, school_nbh, school_nbh, participant_basis, palms)
  
  # getnrow = function(x) {
  #   x = x[[1]]
  #   x = x[rowSums(is.na(x)) == 0, ] 
  #   v = nrow(x)
  #   return(v)
  # }
  # school$nrowgeom = unlist(lapply(school$geometry, FUN = getnrow))
  # school_nbh$nrowgeom = unlist(lapply(school_nbh$geometry, FUN = getnrow))
  # home$nrowgeom = unlist(lapply(home$geometry, FUN = getnrow))
  # home_nbh$nrowgeom = unlist(lapply(home_nbh$geometry, FUN = getnrow))
  # 
  # print(school$nrowgeom)
  # print(school_nbh$nrowgeom)
  # print(home$nrowgeom)
  # print(home_nbh$nrowgeom)
  # at this point we should have a cleaned dataset with only consistent data in all objects
  # cat("\nNumber of unique IDs in all objects:")
  # cat(paste0("\n  participant_basis: ", length(unique(participant_basis$identifier))))
  # cat(paste0("\n  palms: ", length(unique(palms$identifier))))
  # cat(paste0("\n  home: ", length(unique(home$identifier))))
  # cat(paste0("\n  home_nbh: ", length(unique(home_nbh$identifier))))
  # cat(paste0("\n  school: ", length(unique(school$school_id))))
  # cat(paste0("\n  school_nbh: ", length(unique(school_nbh$school_id))))
  # cat("\n")
  invisible(list(palms = palms, participant_basis = participant_basis))
}