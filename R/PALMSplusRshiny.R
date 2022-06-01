#' PALMSplusRshiny
#'
#' @param gisdir Path to directory with GIS files
#' @param palmsdir Path to PALMSpy or PALMS output directory
#' @param gislinkfile Path to participant basis file, which is the file that links all participant identifies with the GIS data
#' @param outputdir Path to outputdir location
#' @param dataset_name Name of dataset
#' @return palms_to_clean_lower object
#' @importFrom stats end start formula as.formula
#' @importFrom tidyr pivot_wider
#' @importFrom readr write_csv read_csv
#' @import palmsplusr
#' @import dplyr
#' @importFrom utils head tail
#' 
#' @export

PALMSplusRshiny <- function(gisdir = "",
                            palmsdir = "",
                            gislinkfile = "",
                            outputdir = "",
                            dataset_name = "") {
  home = school = home_nbh = school_nbh = NULL
  . = lon = identifier = palms = NULL
  
  palmsplus_folder = paste0(outputdir, "/PALMSplus_output")
  if (!dir.exists(palmsplus_folder)) {
    cat("\nCreating PALMSplusR output directory")
    dir.create(palmsplus_folder)
  }
  sf::sf_use_s2(FALSE)
  # identify palms csv output files in palmsdir:
  palms_country_files <- list.files(path = palmsdir, pattern = "*.csv", full.names = TRUE)
  # read and combine the palms csv output files
  csv_palms <- lapply(palms_country_files, FUN = read_csv, col_types = list(
    identifier = readr::col_character(),
    dow = readr::col_integer(),
    lat = readr::col_double(),
    lon = readr::col_double(),
    fixTypeCode = readr::col_integer(),
    iov = readr::col_integer(),
    tripNumber = readr::col_integer(),
    tripType = readr::col_integer(),
    tripMOT = readr::col_integer(),
    activity = readr::col_integer()
  ))
  PALMS_combined <- bind_rows(csv_palms)
  
  # Data cleaning:
  print("start cleaning")
  PALMS_reduced <- subset(PALMS_combined, lon > -180)
  palms_reduced_cleaned <- check_and_clean_palms_data(PALMS_reduced, dataset_name)
  print("cleaning completed")
  
  # Write to csv and read using read_palms to format the object as expected from the rest of the code
  PALMS_reduced_file = paste0(palmsplus_folder, "/", stringr::str_interp("PALMS_${dataset_name}_reduced.csv"))
  print(paste0("Check PALMS_reduced_file: ", PALMS_reduced_file))
  write.csv(palms_reduced_cleaned, PALMS_reduced_file)
  palms = palmsplusr::read_palms(PALMS_reduced_file)
  
  # VvH I have added this:
  find_file = function(path, namelowercase) {
    allcsvfiles = dir(path, recursive = TRUE, full.names = TRUE)
    file_of_interest = allcsvfiles[which(tolower(basename(allcsvfiles)) == namelowercase)]
    return(file_of_interest)
  }
  print("reading basis file")
  participant_basis = read_csv(gislinkfile)
  unique_ids_in_palms <- unique(palms$identifier)
  unique_ids_in_participant_basis <- unique(participant_basis$identifier)
  # VvH - Test for missing values in participant basis
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
  # VvH turned this off because now only process IDs with complete data
  # missing_ids_in_participant_basis <- setdiff(unique_ids_in_palms, unique_ids_in_participant_basis)
  # if(length(missing_ids_in_participant_basis) > 0){
  #   participant_basis <- rbind(participant_basis, data.frame(identifier = missing_ids_in_participant_basis, school_id = NA, class_id = NA))
  #   write.csv(participant_basis, paste(str_interp("participant_basis_${dataset_name}.csv")))
  # }
  
  # Load all shape files ----------------------------------------------------
  hometablefile = find_file(path = gisdir, namelowercase = "home_table.shp")
  schooltablefile = find_file(path = gisdir, namelowercase = "school_table.shp")
  lochomebuffersfile = find_file(path = gisdir, namelowercase = "loc_homebuffers.shp")
  locschoolbuffersfile = find_file(path = gisdir, namelowercase = "loc_schoolbuffers.shp")
  home = sf::read_sf(hometablefile) #
  school = sf::read_sf(schooltablefile)
  home_nbh = sf::read_sf(lochomebuffersfile)
  school_nbh = sf::read_sf(locschoolbuffersfile)
  
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
  
  
  # VvH test for incomplete id in palms object
  missing_identifiers = unique(c(palms$identifier[which(palms$identifier %in% participant_basis$identifier == FALSE)],
                                 participant_basis$identifier[which(participant_basis$identifier %in% palms$identifier == FALSE)]))
  
  if (length(missing_identifiers) > 0) {
    print("Removing missing identifiers related to palms")
    print(missing_identifiers)
    participant_basis = participant_basis[participant_basis$identifier %in% missing_identifiers == FALSE,]
    palms = palms[palms$identifier %in% missing_identifiers == FALSE,]
  }
  check_N(home, home_nbh, school_nbh, school_nbh, participant_basis, palms)
  
  # # VvH test for incomplete home id
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
  
  write.csv(participant_basis, paste0(palmsplus_folder, "/", stringr::str_interp("participant_basis_${dataset_name}.csv"))) # store file for logging purposes only
  
  # Create field tables -----------------------------------------------------
  
  # Note that I have removed the dependency on palmsplusr for this as it
  # involved super assignment operators which seems to be causing issues
  # when run from within another packages. 
  
  #===============
  # Set Defaults:
  print("create default objects")
  # Replacing: palmsplusr::palms_load_defaults(palms_epoch(palms))
  epoch_length <- as.character(palms_epoch(palms))
  # palmsplus_fields_def
  names = c("weekday", "weekend", "indoors", "outdoors",
            "in_vehicle", "inserted", "pedestrian", "bicycle",
            "vehicle", "nonwear", "wear", "sedentary", "light",
            "moderate", "vigorous", "mvpa")
  formulas = c("dow < 6", "dow > 5", "iov == 3", "iov == 1",
               "iov == 2", "fixtypecode == 6", "tripmot == 1",
               "tripmot == 2", "tripmot == 3", "activityintensity < 0",
               "activityintensity >= 0", "activityintensity == 0",
               "activityintensity == 1", "activityintensity == 2",
               "activityintensity == 3", "moderate + vigorous")
  domain_fields = c(rep(FALSE, 9), rep(TRUE, 7))
  for (mi in 1:length(names)) {
    pfd = tibble(name = names[mi], formula = formulas[mi], domain_field = domain_fields[mi])
    if (!exists("palmsplus_fields_def")) {
      palmsplus_fields_def = pfd
    } else{
      palmsplus_fields_def = rbind(palmsplus_fields_def, pfd)
    }
  }
  
  # trajectory_fields
  names = c("mot", "date", "start", "end",
            "duration", "nonwear","wear", "sedentary",
            "light", "moderate", "vigorous", "mvpa",
            "length", "speed")
  formulas = c("first(tripmot)", "first(as.Date(datetime))",
               "datetime[triptype==1]", "datetime[triptype==4]",
               paste0("as.numeric(difftime(end, start, units = \"secs\") + ", epoch_length, ")"),
               paste0("sum(activityintensity < 0) * ", epoch_length),
               paste0("sum(activityintensity >= 0) * ", epoch_length),
               paste0("sum(activityintensity == 0) * ", epoch_length),
               paste0("sum(activityintensity == 1) * ", epoch_length),
               paste0("sum(activityintensity == 2) * ", epoch_length),
               paste0("sum(activityintensity == 3) * ", epoch_length),
               "moderate + vigorous", "as.numeric(st_length(.))",
               "(length / duration) * 3.6")
  after_conversions = c(rep(FALSE, 12), rep(TRUE, 2))
  for (mi in 1:length(names)) {
    tfd = tibble(name = names[mi], formula = formulas[mi], after_conversion = after_conversions[mi])
    if (!exists("trajectory_fields_def")) {
      trajectory_fields_def = tfd
    } else {
      trajectory_fields_def = rbind(trajectory_fields_def, tfd)
    }
  }
  # multimodal_fields
  names = c("duration", "nonwear", "wear", "sedentary", "light",
            "moderate", "vigorous", "mvpa", "length", "speed")
  funcs = c(rep("sum", 9), "mean")
  for (mi in 1:length(names)) {
    mdf = tibble(name = names[mi], func = funcs[mi])
    if (!exists("multimodal_fields_def")) {
      multimodal_fields_def = mdf
    } else {
      multimodal_fields_def = rbind(multimodal_fields_def, mdf)
    }
  }
  #================
  
  
  
  # Replacing:
  # palmsplusr::palms_add_field("at_home", "palms_in_polygon(., filter(home, identifier == i), identifier)")
  # palmsplusr::palms_add_field("at_school", "palms_in_polygon(., filter(school, school_id == participant_basis %>% filter(identifier == i) %>% pull(school_id)))")
  # palmsplusr::palms_add_field("at_home_nbh","palms_in_polygon(., filter(home_nbh, identifier == i), identifier)")
  # palmsplusr::palms_add_field("at_school_nbh", "palms_in_polygon(., filter(school_nbh, school_id == participant_basis %>% filter(identifier == i) %>% pull(school_id)))")
  # palmsplusr::palms_add_domain("home", "at_home")
  # palmsplusr::palms_add_domain("school", "(!at_home & at_school)")
  # palmsplusr::palms_add_domain("transport", "!at_home & !(at_school) & (pedestrian | bicycle | vehicle)")
  # palmsplusr::palms_add_domain("home_nbh","!at_home & !(at_school) & (!pedestrian & !bicycle & !vehicle) & at_home_nbh")
  # palmsplusr::palms_add_domain("school_nbh","!at_home & !(at_school) & (!pedestrian & !bicycle & !vehicle) & !(at_home_nbh) & at_school_nbh")
  # palmsplusr::palms_add_domain("other", "!at_home & !(at_school) & (!pedestrian & !bicycle & !vehicle) & !(at_home_nbh) & !(at_school_nbh)")
  # palmsplusr::palms_add_trajectory_location("home_school", "at_home", "at_school")
  # palmsplusr::palms_add_trajectory_location("school_home", "at_school", "at_home")
  # palmsplusr::palms_add_trajectory_location("home_home", "at_home", "at_home")
  # palmsplusr::palms_add_trajectory_location("school_school", "at_school", "at_school")


  #=============================
  print("adding fields:")
  names = c("at_home", "at_school", "at_home_nbh", "at_school_nbh")
  formulas = c(paste0("palmsInPolygon(polygons = dyplr::filter(home, identifier == i),",
                                 " collapse_var = identifier)"),
               paste0("palmsInPolygon(polygons = dyplr::filter(school, school_id == participant_basis %>%",
                                 " dyplr::filter(identifier == i) %>% pull(school_id)))"),
               paste0("palmsInPolygon(polygons = dyplr::filter(home_nbh, identifier == i),",
                                 " collapse_var = identifier)"),
               paste0("palmsInPolygon(polygons = dyplr::filter(school_nbh, school_id == participant_basis %>%",
                                 " dyplr::filter(identifier == i) %>% pull(school_id)))"))
  for (mi in 1:length(names)) {
    print(paste0("mi: ", mi))
    domain_field = FALSE
    pfi = tibble(name = names[mi], formula = formulas[mi], domain_field = domain_field)
    if (!exists("palmsplus_fields")) {
      print("create new tibble")
      palmsplus_fields = pfi
    } else {
      print("append to tibble")
      # if (ncol(palmsplus_fields) != 3) {
        print(head(palmsplus_fields))
      # }
      palmsplus_fields = rbind(palmsplus_fields, pfi)
    }
    if (length(palmsplus_fields) == 0) {
      palmsplus_fields = palmsplus_fields_def
    }
  }
  #=============================
  print("adding domains:")
  names = c("home","school", "transport", "home_nbh", "school_nbh", "other")
  formulas =  c(paste0("at_home"),
                paste0("(!at_home & at_school)"),
                paste0("!at_home & !(at_school) & (pedestrian | bicycle | vehicle)"),
                paste0("!at_home & !(at_school) & (!pedestrian & !bicycle & !vehicle) & at_home_nbh"),
                paste0("!at_home & !(at_school) & (!pedestrian & !bicycle & !vehicle) &",
                                  " !(at_home_nbh) & at_school_nbh"),
                paste0("!at_home & !(at_school) & (!pedestrian & !bicycle & !vehicle) &",
                                  " !(at_home_nbh) & !(at_school_nbh)"))
  
  for (mi in 1:length(names)) {
    print(paste0("mi: ", mi))
    pdo = tibble(name = names[mi], formula = formulas[mi])
    if (!exists("palmsplus_domains")) {
      print("create new tibble")
      palmsplus_domains = pdo
    } else {
      print("append to tibble")
      # if (ncol(palmsplus_domains) != 2) {
      print(head(palmsplus_domains))
      # }
      palmsplus_domains = rbind(palmsplus_domains, pdo)
    }
  }
  #=============================
  print("adding trajectories")
  names = c("home_school", "school_home", "home_home", "school_school")
  formulas = c("at_home", "at_school",
               "at_home", "at_school")
  for (mi in 1:length(names)) {
    print(paste0("mi: ", mi))
    tfi = tibble(name = names[mi], formula = formulas[mi], after_conversion = "at_home")
    if (!exists("trajectory_fields")) {
      print("create new tibble")
      trajectory_fields = tfi
    } else {
      print("append to tibble")
      # if (ncol(trajectory_fields) != 3) {
      print(head(trajectory_fields))
      # }
      trajectory_fields = rbind(trajectory_fields, tfi)
    }
    if (length(trajectory_fields) == 0) {
      trajectory_fields = trajectory_fields_def
    }
  }
  #=============================
  
  # Run palmsplusr ----------------------------------------------------------
  # overwrite = TRUE
  # fns = c(paste0(palmsplus_folder, "/", dataset_name, "_palmsplus.csv"),
  #         paste0(palmsplus_folder, "/", dataset_name, "_days.csv"),
  #         paste0(palmsplus_folder, "/", dataset_name, "_trajectories.csv"),
  #         paste0(palmsplus_folder, "/", dataset_name, "_multimodal.csv"))
  # if (overwrite == TRUE) {
  #   for (fn in fns) {
  #     if (file.exists(fn)) file.remove(fn)
  #   }
  # }
  
  # this is now an internal function inside HabitusGUI to address scoping problems:
  palmsplus_build(palms = palms,
                  config_file = NULL,
                  verbose = TRUE,
                  spatial_threshold = 200,
                  temporal_threshold = 10,
                  palmsplus_folder = palmsplus_folder,
                  dataset_name = dataset_name,
                  palmsplus_fields = palmsplus_fields,
                  palmsplus_domains = palmsplus_domains,
                  trajectory_fields = trajectory_fields,
                  multimodal_fields_def = multimodal_fields_def,
                  home = home,
                  school = school,
                  home_nbh = home_nbh,
                  school_nbh = school_nbh)
  
  # print("run palmplusr - plus")
  # palmsplus <- palmsplusr::palms_build_palmsplus(palms)
  # write_csv(palmsplus, file = fns[1])
  # 
  # print("run palmplusr - days")
  # days <- palmsplusr::palms_build_days(palmsplus)
  # write_csv(days,  file = fns[2])
  # sf::st_write(palmsplus, dsn = paste0(palmsplus_folder, "/", dataset_name, "_palmsplus.shp"), append = FALSE)
  # 
  # print("run palmplusr - trajectories")
  # trajectories <- palmsplusr::palms_build_trajectories(palmsplus)
  # write_csv(trajectories,  file = fns[3])
  # sf::st_write(trajectories, paste0(palmsplus_folder, "/", dataset_name, "_trajecories.shp"))
  # 
  # print("run palmplusr - multimodal")
  # multimodal <- palmsplusr::palms_build_multimodal(data = trajectories,
  #                                                  spatial_threshold = 200,
  #                                                  temporal_threshold = 10,
  #                                                  palmsplus_copy = palmsplus) # p
  # write_csv(multimodal, file = fns[4])
  # sf::st_write(multimodal, paste0(palmsplus_folder, "/", dataset_name, "_multimodal.shp"))
  
  
  print("end reached")
  return(NULL)
}
########################################