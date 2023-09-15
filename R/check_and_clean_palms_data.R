#' check_and_clean_palms_data
#'
#' @param palms_to_clean palms_to_clean
#' @param country_name country_name
#' @param outputdir outputdir
#' @return palms_to_clean_lower object
#' @import dplyr
#' @export
check_and_clean_palms_data <- function(palms_to_clean, country_name, outputdir = NULL){
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
  
  # Saving the new 'clean'  dataset  - %>% ---------------------------------------
  # write_csv(palms, str_replace(link_to_csv, pattern = '.csv', '_cleaned.csv'), na = "") 
  data.table::fwrite(error_list, paste(outputdir, country_name,"error_list.csv", sep = "_"))
  
  return(palms_to_clean_lower)
}

