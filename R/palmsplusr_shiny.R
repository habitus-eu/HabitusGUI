#' palmsplusr_shiny
#'
#' @param gisdir Path to directory with GIS files
#' @param palmsdir Path to PALMSpy or PALMS output directory
#' @param gislinkfile Path to participant basis file, which is the file that links all participant identifies with the GIS data
#' @param outputdir Path to outputdir location
#' @param dataset_name Name of dataset
#' @param configfile Configuration file
#' @param verbose verbose Boolean
#' @return palms_to_clean_lower object
#' @importFrom stats end start formula as.formula
#' @importFrom tidyr pivot_wider
#' @importFrom readr read_csv
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
                             configfile = "",
                             verbose = TRUE) {
  
  groupinglocation = "school"
  # create list structure to house the location objects
  shapefilenames = dir(path = gisdir, full.names = FALSE, pattern = "[.]shp")
  locationNames = unique(gsub(pattern = "table|_|buffers|[.]|xml|shp|loc", replacement = "", x = shapefilenames))
  
  Nlocations = length(locationNames)
  loca = vector("list", Nlocations)
  names(loca) = locationNames
  for (i in 1:Nlocations) {
    loca[[i]] = vector("list", 4)
    names(loca[[i]]) =  c(locationNames[i], paste0(locationNames[i], "_nbh"), 
                          paste0(locationNames[i], "_tablefile"), paste0(locationNames[i], "_locbufferfile"))
  }
  lon = identifier = palms = NULL # . = was also included, but probably wrong
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
  
  palmsplus_folder = paste0(outputdir, "/palmsplusr_output")
  if (!dir.exists(palmsplus_folder)) {
    if (verbose) cat("\nCreating PALMSplusR output directory\n")
    dir.create(palmsplus_folder)
  }
  sf::sf_use_s2(FALSE)
  # identify palms csv output files in palmsdir:
  palms_country_files <- list.files(path = palmsdir, pattern = "*.csv", full.names = TRUE)
  # read and combine palms csv output files 
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
    activity = readr::col_double()
  ))
  PALMS_combined <- bind_rows(csv_palms)
  # Data cleaning:
  if (verbose) cat("\nstart cleaning\n")
  PALMS_reduced <- subset(PALMS_combined, lon > -180)
  palms_reduced_cleaned <- check_and_clean_palms_data(PALMS_reduced, dataset_name)
  if (verbose) cat("\ncleaning completed\n")
  
  PALMS_reduced$dateTime = as.POSIXct(PALMS_reduced$dateTime, format = "%d/%m/%Y %H:%M:%S", tz = "")
  
  # Write to csv and read using read_palms to format the object as expected from the rest of the code
  PALMS_reduced_file = normalizePath(paste0(palmsplus_folder, "/", stringr::str_interp("PALMS_${dataset_name}_reduced.csv")))
  if (verbose) cat(paste0("\nCheck PALMS_reduced_file: ", PALMS_reduced_file))
  write.csv(palms_reduced_cleaned, PALMS_reduced_file)
  palms = palmsplusr::read_palms(PALMS_reduced_file)
  palms$datetime = as.POSIXct(palms$datetime, format = "%d/%m/%Y %H:%M:%S", tz = "")
  
  # Helper function to find shape files
  find_file = function(path, namelowercase) {
    allcsvfiles = dir(path, recursive = TRUE, full.names = TRUE)
    file_of_interest = allcsvfiles[which(tolower(basename(allcsvfiles)) == namelowercase)]
    return(file_of_interest)
  }
  if (verbose) cat("\nreading basis file\n")
  participant_basis = read_csv(gislinkfile)
  # Load all shape files ----------------------------------------------------
  #----------------
  # NEW CODE
  for (jj in 1:Nlocations) {
    findfile3 = find_file(path = gisdir, namelowercase = paste0(locationNames[jj], "_table.shp"))
    if (!is.null(findfile3)) {
      loca[[jj]][3] = findfile3
    } else {
      stop(paste0("unable to find ", findfile3))
    }
    findfile4 = find_file(path = gisdir, namelowercase = paste0("loc_", locationNames[jj], "buffers.shp"))
    if (!is.null(findfile3)) {
      loca[[jj]][4] = findfile4
    } else {
      stop(paste0("unable to find ", findfile4))
    }
    loca[[jj]][[1]] = sf::read_sf(loca[[jj]][3]) #home_nbh
    loca[[jj]][[2]] = sf::read_sf(loca[[jj]][4]) #school_nbh
  }
  # Force id numbers to be characrer(
  locationNames = names(loca)
  for (i in 1:length(loca)) {
    if (locationNames[i] != "home") {
      loc_id = paste0(groupinglocation, "_id")
    } else  if (locationNames[i] == "home") { # assumption that home is always the identifier for individuals
      loc_id = "identifier"
    } 
    for (j in 1:2) {
      loca[[i]][j][[1]][[loc_id]] = as.character(loca[[i]][j][[1]][[loc_id]])
    }
  }
  # Check for missing IDs -------------------------------------------------------------------------
  withoutMissingId = hbt_check_missing_id(participant_basis, palmsplus_folder, dataset_name, palms,
                                          loca, groupinglocation = groupinglocation,
                                          verbose = verbose)
  palms = withoutMissingId$palms
  participant_basis = withoutMissingId$participant_basis
  loca = withoutMissingId$loca
  write.csv(participant_basis, paste0(palmsplus_folder, "/", stringr::str_interp("participant_basis_${dataset_name}.csv"))) # store file for logging purposes only
  
  
  #===========================================================================================  
  # Create field tables
  # #=============================
  # adding fields
  CONF = read.csv(config, sep = ",")
  CONF$start_criteria = ""
  CONF$end_criteria = ""
  # add standard location based fields to CONF object:
  for (i in 1:Nlocations) {
    if (locationNames[i] == "home") {
      CONF[nrow(CONF) + 1, ] = c("palmsplus_field",
                                 paste0("at_", locationNames[i]), 
                                 paste0("palms_in_polygon(datai, polygons = dplyr::filter(", 
                                        locationNames[i],", identifier == i), identifier)"),
                                 NA, "", "", "")
      CONF[nrow(CONF) + 1, ] = c("palmsplus_field",
                                 paste0("at_", locationNames[i], "_nbh"), 
                                 paste0("palms_in_polygon(datai, polygons = dplyr::filter(",
                                        locationNames[i], "_nbh, identifier == i), identifier)"),
                                 NA, "", "", "")
    } else {
      CONF[nrow(CONF) + 1, ] = c("palmsplus_field",
                                 paste0("at_", locationNames[i]), 
                                 paste0("palms_in_polygon(datai, polygons = dplyr::filter(", 
                                        locationNames[i],",", locationNames[i],
                                        "_id == participant_basis %>% filter(identifier == i) %>% pull(",
                                        locationNames[i], "_id)))"),
                                 NA, "", "", "")
      CONF[nrow(CONF) + 1, ] = c("palmsplus_field",
                                 paste0("at_", locationNames[i], "_nbh"), 
                                 paste0("palms_in_polygon(datai, polygons = dplyr::filter(", 
                                        locationNames[i], "_nbh,", locationNames[i],
                                        "_id == participant_basis %>% filter(identifier == i) %>% pull(",
                                        locationNames[i], "_id)))"),
                                 NA, "", "", "")
    }
    for (j in 1:Nlocations) {
      CONF[nrow(CONF) + 1, ] = c("trajectory_location",
                                 paste0(locationNames[i], "_", locationNames[i]),
                                 paste0("at_", locationNames[i]), NA, "at_home",
                                 paste0("at_", locationNames[i]),
                                 paste0("at_", locationNames[j]))
    }
    CONF = CONF[!duplicated(CONF),]
  }
  palmsplusr_field_rows = which(CONF$context == "palmsplus_field")
  palmsplus_fields = tibble(name = CONF$name[palmsplusr_field_rows],
                            formula = CONF$formula[palmsplusr_field_rows],
                            domain_field = CONF$domain_field[palmsplusr_field_rows])
  
  palmsplusr_domain_rows = which(CONF$context == "palmsplus_domain")
  palmsplus_domains = tibble(name = CONF$name[palmsplusr_domain_rows],
                             formula = CONF$formula[palmsplusr_domain_rows],
                             domain_field = CONF$domain_field[palmsplusr_domain_rows])
  # #=============================
  # # trajectory_fields
  trajectory_field_rows = which(CONF$context == "trajectory_field")
  trajectory_fields = tibble(name = CONF$name[trajectory_field_rows],
                             formula = CONF$formula[trajectory_field_rows],
                             after_conversion = CONF$after_conversion[trajectory_field_rows])
  # #=============================
  # # multimodal_fields
  multimodal_fields_rows = which(CONF$context == "multimodal_field")
  multimodal_fields = tibble(name = CONF$name[multimodal_fields_rows],
                             formula = CONF$formula[multimodal_fields_rows])
  # #=============================
  # # trajectory locations
  trajectory_location_rows = which(CONF$context == "trajectory_location")
  trajectory_locations = tibble(name = CONF$name[trajectory_location_rows],
                                start_criteria = CONF$start_criteria[trajectory_location_rows],
                                end_criteria = CONF$end_criteria[trajectory_location_rows])
  # save(palms, loca, participant_basis, file = "~/projects/fontys/state_1_gui.RData")
  # Run palmsplusr ----------------------------------------------------------
  fns = c(paste0(palmsplus_folder, "/", dataset_name, "_palmsplus.csv"),
          paste0(palmsplus_folder, "/", dataset_name, "_days.csv"),
          paste0(palmsplus_folder, "/", dataset_name, "_trajectories.csv"),
          paste0(palmsplus_folder, "/", dataset_name, "_multimodal.csv"))
  for (fn in fns) {
    if (file.exists(fn)) file.remove(fn)
  }
  
  Nlocation_objects = NULL
  for (i in 1:Nlocations) {
    Nlocation_objects = c(Nlocation_objects, length(loca[[i]][[1]]), length(loca[[i]][[2]]))
  }
  if (verbose) cat("\n<<< building palmsplus...\n")
  if (length(palms) > 0 & length(palmsplus_fields) &
      all(Nlocation_objects > 0) & length(participant_basis) > 0) {
    
    
    palmsplus <- hbt_build_palmsplus(data = palms, 
                                     palmsplus_fields = palmsplus_fields,
                                     loca = loca,
                                     participant_basis = participant_basis,
                                     verbose = verbose)
    data.table::fwrite(palmsplus, file = fns[1])
    if (verbose) cat(">>>\n")
  } else {
    if (verbose) cat("skipped because insufficient input data>>>\n")
  }
  if (verbose) cat("\n<<< building days...")
  if (length(palmsplus) > 0 & length(palmsplus_domains) > 0 & length(palmsplus_fields) &
      all(Nlocation_objects > 0) & length(participant_basis) > 0) {
    days <- hbt_build_days(data = palmsplus,
                           palmsplus_domains = palmsplus_domains,
                           palmsplus_fields = palmsplus_fields,
                           loca = loca,
                           participant_basis = participant_basis,
                           verbose = verbose)
    
    if (length(days) > 0) {
      if (verbose) cat(paste0("  N rows in days object: ", nrow(days)))
      data.table::fwrite(x = days, file = fns[2])
    } else {
      if (verbose) cat(paste0("  WARNING: no days object produced."))
    }
    
    # sf::st_write(palmsplus, dsn = paste0(palmsplus_folder, "/", dataset_name, "_palmsplus.shp"), append = FALSE)
    
  } else {
    if (verbose) cat("skipped because insufficient input data>>>\n")
  }
  if (verbose) cat(">>>\n")
  trajectory_locations = trajectory_locations[order(trajectory_locations$name),]
  if (verbose) cat("\n<<< building trajectories...\n")
  if (length(palmsplus) > 0 & length(trajectory_fields) > 0) {
    
    trajectories <- hbt_build_trajectories(data = palmsplus,
                                           trajectory_fields = trajectory_fields,
                                           trajectory_locations = trajectory_locations)
    if (length(trajectories) > 0) {
      data.table::fwrite(trajectories,  file = fns[3])
      shp_file = paste0(palmsplus_folder, "/", dataset_name, "_trajecories.shp")
      if (file.exists(shp_file)) file.remove(shp_file) # remove because st_write does not know how to overwrite
      
      sf::st_write(obj = trajectories, dsn = shp_file)
      if (verbose) cat(paste0("  N rows in trajectories object: ", nrow(trajectories)))
    } else {
      if (verbose) cat(paste0("  WARNING: no trajectories object produced."))
    }
    if (verbose) cat(">>>\n")
  } else {
    if (verbose) cat("skipped because insufficient input data>>>\n")
  }
  if (verbose) cat("\n<<< building multimodal...\n")
  if (length(palmsplus) > 0 & length(multimodal_fields) > 0 & length(trajectory_locations) > 0) {
    multimodal <- hbt_build_multimodal(data = trajectories,
                                       spatial_threshold = 200,
                                       temporal_threshold = 10,
                                       palmsplus = palmsplus,
                                       multimodal_fields = multimodal_fields,
                                       trajectory_locations = trajectory_locations,
                                       verbose = verbose)
    
    if (length(multimodal) > 0) {
      data.table::fwrite(multimodal, file = fns[4])
      shp_file = paste0(palmsplus_folder, "/", dataset_name, "_multimodal.shp")
      if (file.exists(shp_file)) file.remove(shp_file) # remove because st_write does not know how to overwrite
      sf::st_write(obj = multimodal, dsn = shp_file)
      if (verbose) cat(paste0("  N rows in multimodal object: ", nrow(multimodal)))
    } else {
      if (verbose) cat(paste0("  WARNING: no multimodal object produced."))
    }
    if (verbose) cat(">>>\n")
  } else {
    if (verbose) cat("skipped because insufficient input data>>>\n")
  }
  return()
}
