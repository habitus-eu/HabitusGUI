
#' Build the palmsplus dataset
#'
#' @description Build the \code{palmsplus} dataset by adding additional columns to the PALMS dataset.
#' The additional columns are specified using \code{\link{palms_add_field}}.
#'
#' @param data The PALMS data obtained using \code{\link{read_palms}}.
#' @param verbose Print progress to console after each iteration. Default is \code{TRUE}.
#' @param config_file Path to the config file
#' @param palmsplus_fields fields defined in PALMSplusRshiny
#' @param home home
#' @param school school
#' @param home_nbh home_nbh
#' @param school_nbh school_nbh
#' @param participant_basis participant_basis
#'
#' @import dplyr
#' @import sf
#' @importFrom rlang parse_expr
#' @importFrom stats setNames
#' @importFrom data.table rbindlist
#'
#'
#' @export
hbt_build_palmsplus <- function(data = NULL, config_file = NULL, verbose = TRUE, palmsplus_fields = NULL,
                                home = NULL,
                                school = NULL,
                                home_nbh = NULL,
                                school_nbh = NULL,
                                participant_basis = NULL) {

  
  if (is.null(palmsplus_fields) & is.null(config_file)) stop("No palmsplus fields have been added (and no config file specified)")
  if (is.null(data)) stop("No data provided")
  if (is.null(home)) stop("home object is missing")
  if (is.null(school)) stop("school object is missing")
  if (is.null(home_nbh)) stop("home_nbh object is missing")
  if (is.null(school_nbh)) stop("school_nbh object is missing")
  if (is.null(participant_basis)) stop("participant_basis object is missing")
  
  # If using field tables
  if (is.null(palmsplus_fields) & is.null(config_file)) {
    field_args <- setNames(palmsplus_fields[[2]], palmsplus_fields[[1]]) %>%
      lapply(parse_expr)
  }

  # If using config file
  if (!is.null(config_file)) {

    config <- hbt_read_config(config_file) %>%
      filter(context == 'palmsplus_field')

    field_args <- setNames(config$formula, config$name) %>%
      lapply(parse_expr)
  }


  x <- list()
  j <- 1
  len <- length(unique(data$identifier))

  for (i in unique(data$identifier)) {
    x[[i]] <- data %>%
      filter(identifier == i) %>%
      mutate(!!! field_args) %>%
      mutate_if(is.logical, as.integer)

    if (verbose) {
      cat("[", j, "/", len, "] Computed palmsplus for: ", i, "\n", sep = "")
      j <- j + 1
    }
  }

  data <- rbindlist(x) %>%
    st_set_geometry(data$geometry)
}
