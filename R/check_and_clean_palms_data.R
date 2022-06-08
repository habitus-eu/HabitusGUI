#' check_and_clean_palms_data
#'
#' @param palms_to_clean palms_to_clean
#' @param country_name country_name
#' @return palms_to_clean_lower object
#' @import dplyr
#' @export
check_and_clean_palms_data <- function(palms_to_clean, country_name){
  dif = datetime = identifier = tripnumber = triptype = tt1 = tt4 = NULL
  miss_end = miss_start = multi_start = multi_end = error = NULL
  # Create error list -------------------------------------------------------
  
  error_list <- data.frame(identifier = character(),
                           tripnumber = integer(),
                           error = character(),
                           resolution = character())
  
  
  # Load PALMS dataset ------------------------------------------------------
  palms_to_clean_lower <- palms_to_clean %>%
    rename_all(~tolower(.))
  
  # Checking duplicated rows ------------------------------------------------
  
  # VvH: Following lines do not seem to work
  # palms_dupe <- palms_to_clean_lower %>%
  #   distinct(across(identifier:datetime))
  # VvH: I have now added the following line instead
  palms_dupe = palms_to_clean_lower[!duplicated(palms_to_clean_lower[,c("identifier","datetime")]),]
  if (nrow(palms_to_clean_lower) != nrow(palms_dupe)) {
    dupe <- palms_to_clean_lower %>% dplyr::select(identifier:datetime)
    dupe <- dupe[duplicated(dupe),]
    error_list <- rbind(error_list, data.frame(identifier = dupe$identifier, 
                                               tripnumber = NA,
                                               error = "Duplicated timestamp",
                                               resolution = paste0("Keep one of each duplicate (timestamp: ", dupe$datetime, ")")))
    palms_to_clean_lower <- palms_dupe
  }
  rm(palms_dupe)
  
  # Checking trips less than 1.5 minutes ------------------------------------
  palms_to_clean_lower %>% 
    head(2) %>% 
    summarise(dif = difftime(datetime, lag(datetime))) %>% 
    filter(!is.na(dif)) %>%
    mutate(dif = as.numeric(dif)) %>% 
    pull(dif) -> epoch
  
  if ("tripnumber" %in% colnames(palms_to_clean_lower)) {
    palms_to_clean_lower %>%
      group_by(identifier, tripnumber) %>%
      tally() %>%
      filter(n < ((60/epoch) * 1.5)) -> trip_single
    if (nrow(trip_single) > 0) {
      
      error_list <- rbind(error_list, data.frame(identifier = trip_single$identifier, 
                                                 tripnumber = trip_single$tripnumber,
                                                 error = paste0("Trip less than 1.5 mins: ", trip_single$n, " epochs"),
                                                 resolution = "Set tripnumber to 0 (not a trip)"))
      
      # All trips less than 1 minute are no longer trips.
      for (i in 1:nrow(trip_single)) {
        palms_to_clean_lower[which(palms_to_clean_lower$identifier == trip_single$identifier[i] & palms_to_clean_lower$tripnumber == trip_single$tripnumber[i]), "tripnumber"] <- 0
      }
      
    }
  }
  
  
  
  # Checking for trips missing start and end points -------------------------
  if ("tripnumber" %in% colnames(palms_to_clean_lower)) {
    # Do this twice, just in case
    for (j in 1:2) {
      
      # Identify trips missing start and end points, or have multiple start and end points
      palms_to_clean_lower %>%
        filter(tripnumber > 0) %>% 
        group_by(identifier, tripnumber, triptype) %>%
        tally() %>%
        filter(triptype %in% c(1, 4)) %>%
        pivot_wider(names_from = triptype, names_prefix = 'tt', values_from = n) %>%
        rename(start = tt1, end = tt4) %>%
        filter(is.na(start) |  is.na(end) | start != 1 |  end != 1) %>%
        mutate(miss_start = is.na(start),
               miss_end = is.na(end),
               multi_start = start > 1,
               multi_end = end > 1) -> start_end
      
      # Impute triptype = 4 for trips with missing end points
      missing_end <- start_end %>% filter(miss_end == T)
      
      if (nrow(missing_end) > 0) {
        error_list <- rbind(error_list, data.frame(identifier = missing_end$identifier, 
                                                   tripnumber = missing_end$tripnumber,
                                                   error = "Missing trip end point (triptype = 4)",
                                                   resolution = "Set last point to end point"))
        
        for (i in 1:nrow(missing_end)) {
          palms_to_clean_lower[max(which(palms_to_clean_lower$identifier == missing_end$identifier[i] & palms_to_clean_lower$tripnumber == missing_end$tripnumber[i])), "triptype"] <- 4
        }
      }
      
      
      # Impute triptype = 1 for trips with missing start points
      missing_start <- start_end %>% filter(miss_start == T)
      
      if (nrow(missing_start) > 0) {
        
        error_list <- rbind(error_list, data.frame(identifier = missing_start$identifier, 
                                                   tripnumber = missing_start$tripnumber,
                                                   error = "Missing trip start point (triptype = 1)",
                                                   resolution = "Set first point to start point"))
        
        for(i in 1:nrow(missing_start)){
          palms_to_clean_lower[min(which(palms_to_clean_lower$identifier == missing_start$identifier[i] & palms_to_clean_lower$tripnumber == missing_start$tripnumber[i])), "triptype"] <- 1
        }
      }
    }
    
    # Checking for trips with multiple start or end points --------------------
    
    start_end %>%
      filter(multi_start | multi_end) %>%
      dplyr::select(identifier, tripnumber) %>%
      mutate(error = TRUE) -> multi_errors
    
    if (nrow(multi_errors) > 0) {
      
      error_list <- rbind(error_list, data.frame(identifier = multi_errors$identifier, 
                                                 tripnumber = multi_errors$tripnumber,
                                                 error = "Multiple start or end points",
                                                 resolution = "Remove all points part of trip"))
      
      # Remove trips with multiple
      palms_to_clean_lower <- palms_to_clean_lower %>%
        left_join(multi_errors, by = c('identifier' = 'identifier', 'tripnumber' = 'tripnumber')) %>%
        filter(is.na(error)) %>%
        dplyr::select(-error)
    }
    
    # Checking for non-trip points that have a non-zero triptype --------------
    # Points that do not belong to a trip should have triptype 0
    
    palms_to_clean_lower %>% 
      filter((tripnumber == 0) & (triptype != 0)) -> nontrip_error
    
    if (nrow(nontrip_error) > 0) {
      error_list <- rbind(error_list, data.frame(identifier = nontrip_error$identifier, 
                                                 tripnumber = NA,
                                                 error = "Non-zero triptype for tripnumber 0",
                                                 resolution = paste0("Set triptype to 0 | timestamp: ", nontrip_error$datetime)))
      
      
      palms_to_clean_lower <- palms_to_clean_lower %>% 
        mutate(triptype = if_else(tripnumber == 0, 0L, triptype))
    }
    
    # Find trips missing both start end end points ----------------------------
    
    palms_to_clean_lower %>% 
      filter(tripnumber != 0) %>% 
      group_by(identifier, tripnumber) %>% 
      mutate(start = sum(triptype == 1),
             end = sum(triptype == 4)) %>% 
      summarise(start = first(start), end = first(end)) %>% 
      ungroup() %>% 
      filter(start == 0 & end == 0) -> nothing
    
    if (nrow(nothing) > 0) {
      
      error_list <- rbind(error_list, data.frame(identifier = nothing$identifier, 
                                                 tripnumber = nothing$tripnumber,
                                                 error = "Missing both start and end point",
                                                 resolution = "Remove all points part of trip"))
      
      for (i in 1:nrow(nothing)) {
        palms_to_clean_lower <- palms_to_clean_lower[-which(palms_to_clean_lower$identifier == nothing$identifier[i] & 
                                                              palms_to_clean_lower$tripnumber == nothing$tripnumber[i]), ]
      }
    }
  }
  # Saving the new 'clean'  dataset  - %>% ---------------------------------------
  # write_csv(palms, str_replace(link_to_csv, pattern = '.csv', '_cleaned.csv'), na = "") 
  write_csv(error_list, paste(country_name,"error_list.csv", sep = "_"))
  
  return(palms_to_clean_lower)
}

