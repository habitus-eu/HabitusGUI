#' palmsplusr_shiny
#'
#' @param gisdir Path to directory with GIS files
#' @param palmsdir Path to PALMSpy or PALMS output directory
#' @param gislinkfile Path to participant basis file, which is the file that links all participant identifies with the GIS data
#' @param outputdir Path to outputdir location
#' @param dataset_name Name of dataset
#' @param configfile Configuration file
#' @return palms_to_clean_lower object
#' @importFrom stats end start formula as.formula
#' @importFrom tidyr pivot_wider
#' @importFrom readr write_csv read_csv
#' @import palmsplusr
#' @import dplyr
#' @importFrom utils head tail
#' 
#' @export

palmsplusr_shiny <- function(gisdir = "",
                            palmsdir = "",
                            gislinkfile = "",
                            outputdir = "",
                            dataset_name = "",
                            configfile = "") {
  home = school = home_nbh = school_nbh = NULL
  . = lon = identifier = palms = NULL
  if (length(configfile) > 0) {
    # check for missing parameters, such that palmsplusr can fall back on defaults
    # here the config_pamsplusr file inside the package is assumed to hold all the defaults.
    config_def  = system.file("testfiles_palmsplusr/config_palmsplusr.csv", package = "HabitusGUI")[1]
    params_def = load_params(file = config_def , format = "csv_palmsplusr")
    params_def$id = rownames(params_def) #with(params_def, paste0(context,  "__",  name))
    params = load_params(file = configfile , format = "csv_palmsplusr")
    params$id = rownames(params) #with(params, paste0(context,  "__",  name))
    missingPar = which(params_def$id %in% params$id == FALSE)
    if (length(missingPar) > 0) {
      # update the configfile as provide by the user
      params = rbind(params, params_def[missingPar,])
      params = params[, -which(colnames(params) == "id")]
      update_params(new_params = params, file = configfile, format = "csv_palmsplusr")
    }
    rm(params_def)
    config <- configfile
  } else {
    # If no configfile is provided fall back on default
    config <- system.file("testfiles_palmsplusr/config_palmsplusr.csv", package = "HabitusGUI")[1]
  }

  palmsplus_folder = paste0(outputdir, "/PALMSplus_output")
  if (!dir.exists(palmsplus_folder)) {
    cat("\nCreating PALMSplusR output directory\n")
    dir.create(palmsplus_folder)
  }
  sf::sf_use_s2(FALSE)
  # identify palms csv output files in palmsdir:
  palms_country_files <- list.files(path = palmsdir, pattern = "*.csv", full.names = TRUE)
  # read and combine the palms csv output files
  csv_palms <- lapply(palms_country_files, FUN = readr::read_csv, col_types = list(
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
  cat("\nstart cleaning\n")
  PALMS_reduced <- subset(PALMS_combined, lon > -180)
  palms_reduced_cleaned <- check_and_clean_palms_data(PALMS_reduced, dataset_name)
  cat("\ncleaning completed\n")
  
  # Write to csv and read using read_palms to format the object as expected from the rest of the code
  PALMS_reduced_file = paste0(palmsplus_folder, "/", stringr::str_interp("PALMS_${dataset_name}_reduced.csv"))
  print(paste0("Check PALMS_reduced_file: ", PALMS_reduced_file))
  write.csv(palms_reduced_cleaned, PALMS_reduced_file)
  palms = palmsplusr::read_palms(PALMS_reduced_file)
  
  # Helper function to find shape files
  find_file = function(path, namelowercase) {
    allcsvfiles = dir(path, recursive = TRUE, full.names = TRUE)
    file_of_interest = allcsvfiles[which(tolower(basename(allcsvfiles)) == namelowercase)]
    return(file_of_interest)
  }
  cat("\nreading basis file\n")
  participant_basis = read_csv(gislinkfile)
  unique_ids_in_palms <- unique(palms$identifier)
  unique_ids_in_participant_basis <- unique(participant_basis$identifier)
  
  # Load all shape files ----------------------------------------------------
  hometablefile = find_file(path = gisdir, namelowercase = "home_table.shp")
  schooltablefile = find_file(path = gisdir, namelowercase = "school_table.shp")
  lochomebuffersfile = find_file(path = gisdir, namelowercase = "loc_homebuffers.shp")
  locschoolbuffersfile = find_file(path = gisdir, namelowercase = "loc_schoolbuffers.shp")
  home = sf::read_sf(hometablefile) #
  school = sf::read_sf(schooltablefile)
  home_nbh = sf::read_sf(lochomebuffersfile)
  school_nbh = sf::read_sf(locschoolbuffersfile)
  
  
  # Check for missing IDs -------------------------------------------------------------------------
  withoutMissingId = hbt_check_missing_id(participant_basis, palmsplus_folder, dataset_name, palms,
                                          home, school, home_nbh, school_nbh)
  palms = withoutMissingId$palms
  participant_basis = withoutMissingId$participant_basis
  
  # VvH turned this off because now only process IDs with complete data
  # missing_ids_in_participant_basis <- setdiff(unique_ids_in_palms, unique_ids_in_participant_basis)
  # if(length(missing_ids_in_participant_basis) > 0){
  #   participant_basis <- rbind(participant_basis, data.frame(identifier = missing_ids_in_participant_basis, school_id = NA, class_id = NA))
  #   write.csv(participant_basis, paste(str_interp("participant_basis_${dataset_name}.csv")))
  # }
  write.csv(participant_basis, paste0(palmsplus_folder, "/", stringr::str_interp("participant_basis_${dataset_name}.csv"))) # store file for logging purposes only
  
  
  #===========================================================================================  
  # Create field tables
  
  # Note: I have removed the dependency on palmsplusr for this as it
  # involved super assignment operators which seem to be causing issues,
  # defaults are now taken care of in the config file preparation
  

  # #=============================
  # adding fields
  CONF = read.csv(config, sep = ",")
  palmsplusr_field_rows = which(CONF$context == "palmsplus_field")
  palmsplus_fields = tibble(name = CONF$name[palmsplusr_field_rows],
                            formula = CONF$formula[palmsplusr_field_rows],
                            domain_field = CONF$domain_field[palmsplusr_field_rows])
  # #=============================
  # # trajectory_fields
  # CONF = read.csv(config, sep = "\t")
  # trajectory_field_rows = which(CONF$context == "trajectory_field")
  # trajectory_field = tibble(name = CONF$name[trajectory_field_rows],
  #                               formula = CONF$formula[trajectory_field_rows],
  #                               after_conversions = CONF$after_conversions[trajectory_field_rows])
  # #=============================
  # # multimodal_fields
  multimodal_fields_rows = which(CONF$context == "multimodal_field")
  multimodal_fields = tibble(name = CONF$name[multimodal_fields_rows],
                            func = CONF$formula[multimodal_fields_rows])
  # #=============================
  # # trajectory locations
  trajectory_location_rows = which(CONF$context == "trajectory_location")
  trajectory_locations = tibble(name = CONF$name[trajectory_location_rows],
                               start_criteria = CONF$start_criteria[trajectory_location_rows],
                               end_criteria = CONF$end_criteria[trajectory_location_rows])
  
  
  # Run palmsplusr ----------------------------------------------------------
  
  fns = c(paste0(palmsplus_folder, "/", dataset_name, "_palmsplus.csv"),
          paste0(palmsplus_folder, "/", dataset_name, "_days.csv"),
          paste0(palmsplus_folder, "/", dataset_name, "_trajectories.csv"),
          paste0(palmsplus_folder, "/", dataset_name, "_multimodal.csv"))
  for (fn in fns) {
    if (file.exists(fn)) file.remove(fn)
  }
  

  cat("\n<<< building palmsplus... >>>\n")
  palmsplus <- hbt_build_palmsplus(data = palms, 
                                   config_file = config, 
                                   palmsplus_fields = palmsplus_fields,
                                   home = home,
                                   school = school,
                                   home_nbh = home_nbh,
                                   school_nbh = school_nbh,
                                   participant_basis = participant_basis)
  write_csv(palmsplus, file = fns[1])

  cat("\n<<< building days... >>>\n")
  days <- hbt_build_days(data = palmsplus,
                         palmsplus_fields = palmsplus_fields,
                         home = home,
                         school = school,
                         home_nbh = home_nbh,
                         school_nbh = school_nbh,
                         participant_basis = participant_basis)
  write_csv(days,  file = fns[2])

  
  # sf::st_write(palmsplus, dsn = paste0(palmsplus_folder, "/", dataset_name, "_palmsplus.shp"), append = FALSE)
  
  cat("\n<<< building trajectories... >>>\n")
  trajectories <- hbt_build_trajectories(palmsplus, config_file = config)
  write_csv(trajectories,  file = fns[3])
  
  shp_file = paste0(palmsplus_folder, "/", dataset_name, "_trajecories.shp")
  if (file.exists(shp_file)) file.remove(shp_file) # remove because st_write does not know how to overwrite
  sf::st_write(obj = trajectories, dsn = shp_file)
  
  cat("\n<<< building multimodal... >>>\n")
  
  multimodal <- hbt_build_multimodal(data = trajectories,
                                     spatial_threshold = 200,
                                     temporal_threshold = 10,
                                     palmsplus = palmsplus,
                                     multimodal_fields = multimodal_fields,
                                     trajectory_locations = trajectory_locations)
  if (length(multimodal) > 0) {
    write_csv(multimodal, file = fns[4])
    sf::st_write(multimodal, paste0(palmsplus_folder, "/", dataset_name, "_multimodal.shp"))
  }
  
  return()
}
