
#' Calculate day-level summaries from the palmsplus dataset
#'
#' @description Build a days dataset by summarising \code{palmsplus}
#' by day and person (\code{identifier}). Not all variables in \code{palmsplus}
#' are summarised, only those specified using \code{\link{palms_add_field}} with
#' the argument \code{domain_field = TRUE}. By default, a \code{duration} field
#' is added (e.g., the total minutes per day).
#'
#' All data are summarised by default. However, additional aggragation \emph{domains}
#' can be specified using \code{\link{palms_add_domain}} before building days.
#' Domains are a subset of data, such as during school time. All \code{domain_field}
#' variables will be summarised for each \emph{domain} seperatly.
#'
#' @param data The palmsplus data obtained from \code{\link{palms_build_palmsplus}}.
#' @param verbose Print progress to console. Default is \code{TRUE}.
#' @param config_file Path to the config file
#' @param palmsplus_domains ...
#' @param domain_field ...
#' @param palmsplus_fields ...
#' @param home home
#' @param school school
#' @param home_nbh home_nbh
#' @param school_nbh school_nbh
#' @param participant_basis participant_basis
#'
#'
#' @return A table summarised by day.
#'
#' @import dplyr
#' @import sf
#' @importFrom rlang parse_expr UQ
#' @importFrom purrr reduce
#'
#' @export
hbt_build_days <- function(data = NULL, verbose = TRUE, config_file = NULL, palmsplus_domains = NULL, domain_field = NULL,
                           palmsplus_fields = NULL,
                           home = NULL,
                           school = NULL,
                           home_nbh = NULL,
                           school_nbh = NULL,
                           participant_basis = NULL) {
  duration = datetime = name = NULL
  
  if (is.null(data)) stop("No data provided")
  if (is.null(home)) stop("home object is missing")
  if (is.null(school)) stop("school object is missing")
  if (is.null(home_nbh)) stop("home_nbh object is missing")
  if (is.null(school_nbh)) stop("school_nbh object is missing")
  if (is.null(participant_basis)) stop("participant_basis object is missing")
  
  domains <- "total"
  domain_args <- setNames("1", "total") %>% lapply(parse_expr)
  
  # If using field tables
  if (!is.null(palmsplus_domains)) {
    if (verbose)
      message("palms_build_days: No domains have been added - using totals only.")
    
  } else if (is.null(config_file)) {
    domains <- c(domains, palmsplus_domains[[1]])
    domain_args <- c(domain_args, setNames(palmsplus_domains[[2]], palmsplus_domains[[1]]) %>%
                       lapply(parse_expr))
  }
  
  # If using config file
  if (!is.null(config_file)) {
    config <- hbt_read_config(config_file) %>%
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
  
  
  data <- data %>%
    mutate(!!! domain_args) %>%
    mutate_if(is.logical, as.integer)
  
  
  if (!is.null(config_file)) {
    config <- hbt_read_config(config_file) %>%
      filter(context == 'palmsplus_field')
    
    fields <- config %>% filter(domain_field == TRUE) %>% pull(name)
    
  } else {
    
    fields <- palmsplus_fields %>% filter(domain_field == TRUE) %>% pull(name)
    
  }
  
  data <- data %>%
    st_set_geometry(NULL) %>%
    select(identifier, datetime, domains, fields) %>%
    mutate(duration = 1) %>%
    mutate_at(vars(-identifier,-datetime), ~ . * palms_epoch(data) / 60) %>%
    group_by(identifier, date = as.Date(datetime)) %>%
    select(-datetime)
  
  x <- list()
  for (i in domains) {
    x[[i]] <- data %>%
      filter(UQ(as.name(i)) > 0) %>%
      select(-one_of(domains), duration) %>%
      summarise_all(~ sum(.)) %>%
      ungroup() %>%
      rename_at(vars(-identifier, -date), ~ paste0(i, "_", .))
  }
  
  result <- x %>%
    reduce(left_join, by = c("identifier" = "identifier", "date" = "date"))
  
  return(result)
}
