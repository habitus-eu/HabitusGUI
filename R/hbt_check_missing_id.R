#' hbt_check_missing_id
#'
#' @param participant_basis object as loaded inside PALMSplusRshiny
#' @param palmsplus_folder output folder path for storing log of excluded IDs
#' @param dataset_name Name of dataset
#' @param palms palms object as loaded inside PALMSplusRshiny
#' @param home home
#' @param school school
#' @param home_nbh home_nbh
#' @param school_nbh school_nbh
#' @return List with participant_basis and palms object without non-matching IDs
#'
#' @export
#' 
hbt_check_missing_id = function(participant_basis, palmsplus_folder, dataset_name, palms,
                                home, school, home_nbh, school_nbh) {
  # Test for missing values in participant basis
  test_missing_value = rowSums(is.na(participant_basis[,c("identifier", "school_id")]))
  missing = which(test_missing_value > 1)
  print(paste0("length missing: ", length(length(missing))))
  participant_exclude_list = list(identifier = NULL, school_id = NULL)
  if (length(missing) > 0) {
    print("\nMissing ID values in participant_basis\n")
    print(paste0("\nIgnoring identifier ", paste(participant_basis$identifier[missing], sep = " ")))
    print(paste0("\nIgnoring schoolid ", paste(participant_basis$school_id[missing], sep = " ")))
    participant_exclude_list$identifier = participant_basis$identifier[missing]
    participant_exclude_list$school_id = participant_basis$school_id[missing]
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
    print("Removing missing identifiers related to palms")
    print(missing_identifiers)
    participant_basis = participant_basis[participant_basis$identifier %in% missing_identifiers == FALSE,]
    palms = palms[palms$identifier %in% missing_identifiers == FALSE,]
  }
  
  
  # Check whether id is found in all objects
  check_N = function(home, home_nbh, school, school_nbh, participant_basis, palms) {
    if (length(unique(school$school_id)) == 0) {
      print("No school_id found in school$school_id")
    }
    if (length(unique(school_nbh$school_id)) == 0) {
      print("No school_id found in school_nbh$school_id")
    }
    if (length(unique(home$identifier)) == 0) {
      print("No identifier found in home$identifier")
    }
    if (length(unique(home_nbh$identifier)) == 0) {
      print("No identifier found in home_nbh$identifier")
    }
    if (length(unique(participant_basis$identifier)) == 0) {
      print("No identifier found in participant_basis$identifier")
    }
    if (length(unique(participant_basis$school_id)) == 0) {
      print("No school_id found in participant_basis$school_id")
    }
    if (length(unique(palms$identifier)) == 0) {
      print("No identifier found in palms$identifier")
    }
  }
  check_N(home, home_nbh, school_nbh, school_nbh, participant_basis, palms)
  # at this point we should have a cleaned dataset with only consistent data in all objects
  print("Number of unique IDs in all objects")
  print(paste0("Number of unique IDs in participant_basis: ", length(unique(participant_basis$identifier))))
  print(paste0("palms ", length(unique(palms$identifier))))
  print(paste0("home ", length(unique(home$identifier))))
  print(paste0("home_nbh ", length(unique(home_nbh$identifier))))
  print(paste0("school ", length(unique(school$school_id))))
  print(paste0("school_nbh ", length(unique(school_nbh$school_id))))
  check_N(home, home_nbh, school_nbh, school_nbh, participant_basis, palms)
  
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
  print("Number of unique IDs in all objects")
  print(paste0("participant_basis ", length(unique(participant_basis$identifier))))
  print(paste0("palms ", length(unique(palms$identifier))))
  print(paste0("home ", length(unique(home$identifier))))
  print(paste0("home_nbh ", length(unique(home_nbh$identifier))))
  print(paste0("school ", length(unique(school$school_id))))
  print(paste0("school_nbh ", length(unique(school_nbh$school_id))))
  
  invisible(list(palms = palms, participant_basis = participant_basis))
}