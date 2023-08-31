#' hbt_check_missing_id
#'
#' @param participant_basis object as loaded inside PALMSplusRshiny
#' @param palmsplus_folder output folder path for storing log of excluded IDs
#' @param dataset_name Name of dataset
#' @param palms palms object as loaded inside PALMSplusRshiny
#' @param loca Nested list with location information
#' @param groupinglocation groupinglocation
#' @param verbose verbose
#' @return List with participant_basis and palms object without non-matching IDs
#'
#' @export
#' 
hbt_check_missing_id = function(participant_basis, palmsplus_folder, dataset_name, palms,
                                loca, groupinglocation = "school", verbose = TRUE) {
  
  # Check whether id is found in all objects
  check_N = function(loca, participant_basis, palms, groupinglocation, verbose) {
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
        if (verbose) {
          if (N == 0) {
            cat(paste0("\nNo ",loc_id ," found in ", tibblename, "$", loc_id))
          } else {
            cat(paste0("\n  ", tibblename, ": ", N))
          }
        }
      }
    }
    if (verbose) {
      # Check participant_basis
      if (length(unique(participant_basis[[paste0(groupinglocation, "_id")]])) == 0) {
        cat(paste0("\nNo ", groupinglocation, "_id found in participant_basis$", groupinglocation, "_id"))
      }
      
      # NOT CHANGED:
      Npbi = length(unique(participant_basis$identifier))
      if (Npbi == 0) {
        cat("\nNo identifier found in participant_basis$identifier")
      }
      cat(paste0("\n  participant_basis: ", Npbi))
      
      Npi = length(unique(palms$identifier))
      if (Npi == 0) {
        cat("\nNo identifier found in palms$identifier")
      } else {
        cat(paste0("\n  palms: ", Npi))
      }
    }
    # cat("\n")
  }
  
  locationNames = names(loca)
  locationNames2 = NULL
  if (groupinglocation %in% locationNames) {
    locationNames2 = locationNames[which(locationNames == groupinglocation)] # focus here on school
  } else {
    locationNames2 = locationNames
  }
  loc_id = paste0(locationNames[which(names(participant_basis) %in% paste0(locationNames2, "_id"))][1], "_id")
  
  # Check sample size before cleaning
  if (verbose) cat("\nSample size before cleaning:")
  check_N(loca, participant_basis, palms, groupinglocation, verbose)
  
  #========================================================
  # Test for missing values in participant basis
  test_missing_value = rowSums(is.na(participant_basis[,c("identifier", loc_id)])) #"school_id"
  missing = which(test_missing_value > 1)
  participant_exclude_list = list(identifier = NULL, loc_id = NULL)
  names(participant_exclude_list)[2] = loc_id
  
  if (length(missing) > 0) {
    if (verbose) {
      cat(paste0("\n(MISSING) identifier or ", loc_id, " values in participant_basis\n"))
      cat(paste0("  Now ignoring ", paste(participant_basis$identifier[missing], sep = " ")))
    }
    participant_exclude_list = participant_basis[[missing]]
    participant_basis = participant_basis[test_missing_value == 0, ]
  } else {
    if (verbose) {
      cat(paste0("\n(COMPLETE) identifier and ", loc_id, " values in participant_basis"))
    }
  }
  
  # Commented out, because there are more ids excluded then just these
  # sink(paste0(palmsplus_folder, "/", dataset_name, "_excluded_ids.txt"))
  # print(participant_exclude_list)
  # sink()
  # rm(missing)
  
  #========================================================
  # Make sure home, home_nbh, participant_basis have matching identifier numbers
  # Make sure school, school_nbh, participant_basis have matching school_id numbers
  for (k in locationNames) {
    for (i in c(k, paste0(k, "_nbh"))) {
      if (k == "home")  {
        idloc = "identifier"
      } else {
        idloc = paste0(k,"_id")
      }
      if (k == i) {
        tmp1 = loca[[k]][[i]]
        idset1 = loca[[k]][[i]][[idloc]]
      } else {
        tmp2 = loca[[k]][[i]]
        idset2 = loca[[k]][[i]][[idloc]]
      }
    }
    missing_identifiers = sort(unique(c(idset1[idset1 %in% idset2 == FALSE], idset2[idset2 %in% idset1 == FALSE])))
    if (length(missing_identifiers) > 0) {
      if (verbose) {
        cat(paste0("\n(MISSING) identifier(s) in ", k, " or ", k, "_nbh\n"))
        cat(paste0("  Now ignoring: ", paste(missing_identifiers, collapse = ", "), sep = " "))
      }
      participant_basis = participant_basis[which(participant_basis[[idloc]] %in% missing_identifiers == FALSE),]
      if (idloc %in% names(palms)) {
        palms = palms[which(palms[[idloc]] %in% missing_identifiers == FALSE),]
      }
      for (i in c(k, paste0(k, "_nbh"))) {
        tmp = loca[[k]][[i]]
        validrows = which(tmp[[idloc]] %in% missing_identifiers == FALSE)
        loca[[k]][[i]] <- eval(parse(text = paste0("tmp[validrows,] %>% arrange(", idloc, ")")))
      }
    } else {
      if (verbose) cat(paste0("\n(COMPLETE) identifier(s) in ", k, " and ", k, " _nbh"))
    }
  }

  #========================================================
  # Test for incomplete id in palms object
  missing_identifiers = unique(c(palms$identifier[which(palms$identifier %in% participant_basis$identifier == FALSE)],
                                 participant_basis$identifier[which(participant_basis$identifier %in% palms$identifier == FALSE)]))
  if (length(missing_identifiers) > 0) {
    if (verbose) {
      cat(paste0("\n(MISSING) identifier(s) in palms\n"))
      cat(paste0("  Now ignoring: ", paste(missing_identifiers, collapse = ", "), sep = " "))
    }
    participant_basis = participant_basis[participant_basis$identifier %in% missing_identifiers == FALSE,]
    palms = palms[palms$identifier %in% missing_identifiers == FALSE,]
  } else {
    if (verbose) cat(paste0("\n(COMPLETE) identifier(s) in palms"))
  }
  # Check sample size aftercleaning
  if (verbose) cat("\nSample size after cleaning:")
  check_N(loca, participant_basis, palms, groupinglocation, verbose)
  


  # at this point we should have a cleaned dataset with only consistent data in all objects
  if (verbose) cat("\n")
  
  invisible(list(palms = palms, participant_basis = participant_basis, loca = loca))
}