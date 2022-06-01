#' Build the palmsplus objects
#'
#' @description Build the \code{palmsplus} objects by adding additional columns..
#' @param palms The PALMS data obtained using \code{\link{read_palms}}.
#' @param verbose Print progress to console after each iteration. Default is \code{TRUE}.
#' @param config_file Path to the config file
#' @param spatial_threshold Spatial threshold in meters
#' @param temporal_threshold Temporal threshold in minutes
#' @param palmsplus_folder path to direcotry where palmsplus results should be stored
#' @param dataset_name ...
#' @param palmsplus_fields ...
#' @param palmsplus_domains ...
#' @param trajectory_fields ...
#' @param multimodal_fields_def ...
#' @param home ...
#' @param school ...
#' @param home_nbh ...
#' @param school_nbh ...
#' 
#' @import dplyr
#' @import sf
#' @importFrom rlang parse_expr  UQ
#' @importFrom data.table rbindlist
#' @importFrom purrr reduce
#' @importFrom stats setNames
#' @importFrom geosphere distGeo
#' @importFrom data.table rleid
#' @importFrom tidyr gather spread unite
#'
#' @export
palmsplus_build <- function(palms, config_file = NULL, verbose = TRUE,
                            spatial_threshold,
                            temporal_threshold,
                            palmsplus_folder,
                            dataset_name = "",
                            palmsplus_fields,
                            palmsplus_domains,
                            trajectory_fields,
                            multimodal_fields_def,
                            home,
                            school,
                            home_nbh,
                            school_nbh) {
  print(str(palms))
  print("---------------")
  # Create empty objects to reassure R that object names are expected
  after_conversion = context = datetime = distance_diff = domain_field = duration = NULL
  end_point = end_prev = end_trip = func = geometry = identifier = mmt_criteria = NULL
  mmt_number = mot = name = palmsplus_copy = read_config = start_point = start_trip = NULL
  time_diff = tripnumber = triptype = value = variable = NULL
  
  
  # rewriting function such that objects are by default present inside the function
  palmsInPolygon <- function(polygons, collapse_var = NULL, home = home, school = school,
                             home_nbh = home_nbh, school_nbh = school_nbh){
    . <- NULL # declaring because otherwise R check complains about undefined '.'
    if (nrow(polygons) < 1) {
      message("palms_in_polygon: Polygon data has 0 rows, returning NA")
      return(NA)
    }
    polygons <- sf::st_make_valid(polygons)
    collapse_var <- rlang::quo_text(rlang::enquo(collapse_var))
    if (!(collapse_var == "NULL")) {
      polygons <- aggregate(polygons, list(polygons[[collapse_var]]), function(x) x[1])
    }
    # Supresses the 'planar coordinates' warning
    polygons = suppressMessages(sf::st_contains(x = polygons, y = ., sparse = FALSE) %>% as.vector(.))
    
    return(polygons)
  }
  
  
  print("starting palmsplus_build")
  # palms_build_palmsplus <- function(data, config_file = NULL, verbose = TRUE) {
  print("run palmplusr - plus")
  if (!exists("palmsplus_fields") & is.null(config_file)) stop("No palmsplus fields have been added (and no config file specified)")
  print("a")
  # If using field tables
  if (exists("palmsplus_fields") & is.null(config_file)) {
    print("item 1")
    print(palmsplus_fields[[1]])
    print("item 2")
    print(palmsplus_fields[[2]])
    field_args <- setNames(palmsplus_fields[[2]], palmsplus_fields[[1]]) %>%
      lapply(parse_expr)
  }
  print("b")
  print(config_file)
  # If using config file
  if (!is.null(config_file)) {
    print("b1")
    config <- read_config(config_file) %>%
      filter(context == 'palmsplus_field')
    print("b2")
    field_args <- setNames(config$formula, config$name) %>%
      lapply(parse_expr)
    print("b3")
  }
  
  print("c")
  print(str(palms))
  print("c1")
  print(class(palms))
  print("c2")
  print(head(palms))
  print("c3")
  x <- list()
  j <- 1
  print("c4")
  len <- length(unique(palms$identifier))
  print("c5")
  for (i in unique(palms$identifier)) {
    print("c6")
    x[[i]] <- palms %>%
      dplyr::filter(identifier == i) %>%
      dplyr::mutate(!!! field_args) %>%
      dplyr::mutate_if(is.logical, as.integer)
    print("c7")
    if (verbose) {
      cat("[", j, "/", len, "] Computed palmsplus for: ", i, "\n", sep = "")
      j <- j + 1
    }
  }
  print("d")
  
  palmsplus <- rbindlist(x) %>%
    sf::st_set_geometry(palms$geometry)
  
  # store results
  fn = paste0(palmsplus_folder, "/", dataset_name, "_palmsplus.csv")
  write_csv(palmsplus, file = fn)
  ##############################################
  # palms_build_days <- function(data, verbose = TRUE, config_file = NULL) {
  print("run palmplusr - days")
  domains <- "total"
  domain_args <- setNames("1", "total") %>% lapply(parse_expr)
  print("e")
  # If using field tables
  if (!exists("palmsplus_domains")) {
    
    if (verbose)
      message("palms_build_days: No domains have been added - using totals only.")
    
  } else if (is.null(config_file)) {
    domains <- c(domains, palmsplus_domains[[1]])
    domain_args <- c(domain_args, setNames(palmsplus_domains[[2]], palmsplus_domains[[1]]) %>%
                       lapply(parse_expr))
  }
  print("f")
  # If using config file
  if (!is.null(config_file)) {
    config <- read_config(config_file) %>%
      filter(context == 'palmsplus_domain')
    
    if (nrow(config) < 1) {
      
      #if (verbose)
      #  message("palms_build_days: No domains have been added - using totals only.")
      
    } else {
      domains <- c(domains, config$name)
      domain_args <- c(domain_args, config$formula, config$name) %>%
        lapply(parse_expr)
    }
    
  }
  
  print("g")
  palmsplus_tmp <- palmsplus %>%
    dplyr::mutate(!!! domain_args) %>%
    mutate_if::mutate_if(is.logical, as.integer)
  
  
  if (!is.null(config_file)) {
    config <- read_config(config_file) %>%
      filter(context == 'palmsplus_field')
    
    fields <- config %>% filter(domain_field == TRUE) %>% dplyr::pull(name)
    
  } else {
    
    fields <- palmsplus_fields %>% filter(domain_field == TRUE) %>% dplyr::pull(name)
    
  }
  print("h")
  palmsplus_tmp <- palmsplus_tmp %>%
    st_set_geometry(NULL) %>%
    select(identifier, datetime, domains, fields) %>%
    mutate(duration = 1) %>%
    mutate_at(vars(-identifier,-datetime), ~ . * palms_epoch(palmsplus_tmp) / 60) %>%
    group_by(identifier, date = as.Date(datetime)) %>%
    select(-datetime)
  
  x <- list()
  for (i in domains) {
    x[[i]] <- palmsplus_tmp %>%
      filter(UQ(as.name(i)) > 0) %>%
      select(-one_of(domains), duration) %>%
      summarise_all(~ sum(.)) %>%
      ungroup() %>%
      rename_at(vars(-identifier, -date), ~ paste0(i, "_", .))
  }
  print("i")
  days <- x %>%
    reduce(left_join, by = c("identifier" = "identifier", "date" = "date"))
  
  #   return(result)
  # }
  
  # store results
  fn = paste0(palmsplus_folder, "/", dataset_name,  "_days.csv")
  write_csv(days,  file = fn)
  sf::st_write(palmsplus, dsn = fn, append = FALSE)
  ##########################################
  # palms_build_trajectories <- function(data, config_file = NULL) {
  
  print("run palmplusr - trajectories")
  # If using field tables
  if (exists("trajectory_fields") & is.null(config_file)) {
    args <- trajectory_fields %>% filter(after_conversion == FALSE)
    args_after <- trajectory_fields %>% filter(after_conversion == TRUE)
    
    args <- setNames(args[[2]], args[[1]]) %>% lapply(parse_expr)
    args_after <- setNames(args_after[[2]], args_after[[1]]) %>% lapply(parse_expr)
  } else {
    args <- list()
    args_after <- list()
  }
  print("j")
  # If using config file
  if (!is.null(config_file) & !exists("trajectory_fields")) {
    config <- read_config(config_file) %>%
      filter(context == 'trajectory_field')
    
    if (nrow(config) > 0) {
      args <- config %>% filter(after_conversion == FALSE)
      args_after <- config %>% filter(after_conversion == TRUE)
      
      args <- setNames(args$formula, args$name) %>% lapply(parse_expr)
      args_after <- setNames(args_after$formula, args_after$name) %>% lapply(parse_expr)
    } else {
      args <- list()
      args_after <- list()
    }
  }
  
  print("k")
  # If using field tables
  if (exists("trajectory_locations") & is.null(config_file)) {
    args_locations <- setNames(paste0("first(", trajectory_locations[[2]],
                                      ") & last(", trajectory_locations[[3]], ")"),
                               trajectory_locations[[1]]) %>% lapply(parse_expr)
    args <- c(args, args_locations)
  }
  
  
  # If using config file
  if (!is.null(config_file) & !exists("trajectory_fields")) {
    config <- read_config(config_file) %>%
      filter(context == 'trajectory_location')
    
    if (nrow(config) > 0) {
      args_locations <- setNames(paste0("first(", config$start_criteria,
                                        ") & last(", config$end_criteria, ")"),
                                 config$name) %>% lapply(parse_expr)
      args <- c(args, args_locations)
    }
    
  }
  print("l")
  
  trajectories = palmsplus %>%
    filter(tripnumber > 0) %>%
    group_by(identifier, tripnumber) %>%
    summarise(!!!args, do_union = FALSE) %>%
    st_cast("LINESTRING") %>%
    mutate(!!!args_after) %>%
    ungroup() %>%
    mutate_if(is.logical, as.integer)
  # }
  print("m")
  # store results
  fn = paste0(palmsplus_folder, "/", dataset_name,  "_trajectories.csv")
  write_csv(trajectories,  file = fn)
  sf::st_write(trajectories, fn)
  ###############################
  
  # palms_build_multimodal <- function(data,
  #                                    spatial_threshold,
  #                                    temporal_threshold,
  #                                    palmsplus = NULL,
  #                                    verbose = TRUE,
  #                                    config_file = NULL,
  #                                    palmsplus_copy = NULL) {
  print("run palmplusr - multimodal")
  if (!all(c("identifier", "tripnumber", "start", "end", "geometry", "mot") %in% colnames(trajectories)))
    stop("Your trajectories data does not contain the required column names...")
  
  if(verbose) cat('Calculating multimodal eligibility...')
  print("n")
  # Determine if a trajectory meets spatial and temporal criteria
  trajectories <- trajectories %>%
    arrange(identifier, tripnumber) %>%
    mutate(time_diff = difftime(start, lag(end), units = "mins")) %>%
    group_by(identifier, tripnumber) %>%
    mutate(start_point = st_as_text(st_cast(geometry, "POINT")[1]),
           end_point = st_as_text(st_cast(geometry, "POINT")[length(st_cast(geometry, "POINT"))])) %>%
    ungroup() %>%
    mutate(end_prev = lag(end_point, default = start_point[1])) %>%
    group_by(identifier, tripnumber) %>%
    mutate(distance_diff = distGeo(
      matrix(c(st_as_sfc(end_prev, crs = 4326)[[1]][1],
               st_as_sfc(end_prev, crs = 4326)[[1]][2]), ncol = 2),
      matrix(c(st_as_sfc(start_point, crs = 4326)[[1]][1],
               st_as_sfc(start_point, crs = 4326)[[1]][2]), ncol = 2))) %>%
    ungroup() %>%
    mutate(mmt_criteria = ((distance_diff < spatial_threshold) & (time_diff < temporal_threshold)),
           mmt_number = NA)
  
  if(verbose) cat('done\nAssigning trip numbers...')
  print("o")
  # Assign correct start times for consecutive mmt segments
  for(i in 1:(nrow(trajectories)-1)) {
    trajectories$mmt_number[i] <- ifelse((!trajectories$mmt_criteria[i]) & trajectories$mmt_criteria[i+1], trajectories$start[i],
                                         ifelse(trajectories$mmt_criteria[i], trajectories$mmt_number[i-1], trajectories$start[i]))
  }
  
  trajectories$mmt_number[nrow(trajectories)] <- ifelse(trajectories$mmt_criteria[nrow(trajectories)], trajectories$mmt_number[nrow(trajectories)-1],
                                                        trajectories$start[nrow(trajectories)])
  print("p")
  # Use run-length encoding to assign mmt numbers
  trajectories <- trajectories %>%
    group_by(identifier) %>%
    mutate(mmt_number = data.table::rleid(mmt_number)) %>%
    ungroup() %>%
    select(-c(start_point, end_point, end_prev, mmt_criteria, time_diff, distance_diff))
  
  if(verbose) cat('done\nCalculating fields...')
  
  print("q")
  
  
  if (!exists("multimodal_fields") & !is.null(config_file)) {
    multimodal_fields <- read_config(config_file) %>%
      filter(context == 'multimodal_field')
  } else if (exists("multimodal_fields")) {
    multimodal_fields <- multimodal_fields %>%
      rename(formula = func)
  }
  
  
  if (exists("multimodal_fields")) {
    print("r")
    # Split varables into each mot
    mot_split <- trajectories %>%
      select(c("mot", "mmt_number", "identifier", "geometry", multimodal_fields$name)) %>%
      mutate(mot = paste0("mot_", mot)) %>%
      gather(variable, value, -mmt_number, -mot, -identifier, -geometry) %>%
      unite(col, mot, variable) %>%
      spread(col, value) %>%
      arrange(identifier, mmt_number) %>%
      cbind(trajectories) %>%
      select(-ends_with(".1"))
    
    # Calculate multimodal_fields
    df_fields <- list()
    
    for (i in unique(multimodal_fields$formula)) {
      df_fields[[i]] <- mot_split %>%
        as.data.frame() %>%
        group_by(identifier, mmt_number) %>%
        summarise_at(vars(matches(
          paste(multimodal_fields$name[multimodal_fields$formula == i], collapse = "|"))),
          i, na.rm = TRUE)
    }
    print("s")
    df_fields <- reduce(df_fields, left_join,
                        by = c("identifier" = "identifier", "mmt_number" = "mmt_number"))
    
    df_fields[is.na(df_fields)] <- NA
    
  } else {
    mot_split <- trajectories
  }
  
  print("t")
  
  if (!exists("trajectory_locations") & !is.null(config_file)) {
    
    trajectory_locations <- read_config(config_file) %>%
      filter(context == 'trajectory_location')
    
    if (nrow(trajectory_locations) < 1) {
      rm(trajectory_locations)
    }
  }
  
  print("u")
  # Build trajectory_location formulas if they exist
  if (exists("trajectory_locations")) {
    
    names <- unique(c(trajectory_locations$start_criteria,
                      trajectory_locations$end_criteria))
    
    
    # Rather than recalculating geometry, just lookup in palmsplus
    if (!is.null(palmsplus_copy) | exists("palmsplus")) {
      
      if (is.null(palmsplus_copy)) {
        
        lookup <- palmsplus %>%
          filter(tripnumber > 0 & triptype %in% c(1, 4)) %>%
          as.data.frame() %>%
          select(c("identifier", "tripnumber", "triptype", all_of(names)))
        
      } else {
        lookup <- palmsplus_copy %>%
          filter(tripnumber > 0 & triptype %in% c(1, 4)) %>%
          as.data.frame() %>%
          select(c("identifier", "tripnumber", "triptype", all_of(names)))
        
      }
      
      # Helper function to lookup start and end locations from the lookup table
      lookup_locations <- function(identifier, start_trip, start_loc, end_trip, end_loc) {
        
        n1 <- lookup[(lookup$identifier == identifier) & (lookup$tripnumber == start_trip) & (lookup$triptype == 1), start_loc]
        n2 <- lookup[(lookup$identifier == identifier) & (lookup$tripnumber == end_trip) & (lookup$triptype == 4), end_loc]
        
        return(n1 & n2)
      }
      print("v")
      
      args_locations <- setNames(
        
        paste0("lookup_locations(identifier, start_trip, '", trajectory_locations$start_criteria, "', end_trip, '", trajectory_locations$end_criteria, "')"),
        
        trajectory_locations$name) %>%
        lapply(parse_expr)
      
    } else {
      message("palms_build_multimodal: trajectory_locations config table exists, but cannot find 'palmsplus' dataframe
               in global  environment. Please assign output of palms_build_palmsplus to 'palmsplus'.")
      
      args_locations <- NULL
    }
    print("w")
    
  } else {
    args_locations <- NULL
  }
  
  print("x")
  # Calculate other fields (+ trajectory_locations)
  df_other <- mot_split %>%
    group_by(identifier, mmt_number) %>%
    summarise(start_trip = first(tripnumber),
              end_trip = last(tripnumber),
              trip_numbers = paste0(tripnumber, collapse = "-"),
              n_segments = n(),
              mot_order = paste0(mot, collapse = "-"),
              start = first(start),
              end = last(end),
              do_union = FALSE) %>%
    rowwise() %>%
    mutate(!!!args_locations) %>%
    ungroup() %>%
    select(-c(start_trip, end_trip)) %>%
    mutate_if(is.logical, as.integer)
  
  if (exists("df_fields")) {
    df <- reduce(list(df_other, df_fields), left_join, by = c("identifier" = "identifier", "mmt_number" = "mmt_number"))
  } else {
    df <- df_other
  }
  print("y")
  if(verbose) cat('done\n')
  
  multimodal = df
  if (length(multimodal) == 0) {
    multimodal = multimodal_fields_def
  }
  print("z")
  # store results
  fn = paste0(palmsplus_folder, "/", dataset_name,  "_multimodal.csv")
  write_csv(multimodal, file = fn)
  sf::st_write(multimodal, fn)
  #   return(df)
  # }
}