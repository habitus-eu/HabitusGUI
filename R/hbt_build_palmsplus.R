
#' Build the palmsplus dataset
#'
#' @description Build the \code{palmsplus} dataset by adding additional columns to the PALMS dataset.
#' The additional columns are specified using \code{\link{palms_add_field}}.
#'
#' @param data The PALMS data obtained using \code{\link{read_palms}}.
#' @param verbose Print progress to console after each iteration. Default is \code{TRUE}.
#' @param palmsplus_fields fields defined in PALMSplusRshiny
#' @param loca Nested list with location information
#' @param participant_basis participant_basis
#'
#' @import dplyr
#' @import sf
#' @import palmsplusr
#' @importFrom rlang parse_expr
#' @importFrom stats setNames
#' @importFrom data.table rbindlist
#'
#'
#' @export
#' 
# Code modified from https://thets.github.io/palmsplusr/
hbt_build_palmsplus <- function(data = NULL, verbose = TRUE, palmsplus_fields = NULL,
                                loca = NULL,
                                participant_basis = NULL) {
  # Note:
  # home, school, home_nbh, school_nbh (or similar) need to be present, 
  # because the functions that are passed on assume that they exist
  # So, now we need to create those objects from object loca
  Nlocations = length(loca)
  for (i in 1:Nlocations) {
    for (j in 1:2) {
      txt = paste0(names(loca[[i]])[j], " = loca[[i]][[j]]")
      eval(parse(text = txt))
    }
  }
  field_args <- setNames(palmsplus_fields$formula, palmsplus_fields$name) %>%
    lapply(parse_expr)
  
  x <- list()
  j <- 1
  len <- length(unique(data$identifier))
  
  for (i in unique(data$identifier)) {
    datai = data %>%
      filter(identifier == i)
    x[[i]] <- datai %>%
      mutate(!!! field_args) %>%
      mutate_if(is.logical, as.integer)
    if (verbose) {
      cat("[", j, "/", len, "] Computed palmsplus for: ", i, "\n", sep = "")
      j <- j + 1
    }
  }
  data <- rbindlist(x) %>%
    st_set_geometry(data$geometry)
  return(data)
}
