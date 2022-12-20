
#' Build trajectories from the palmsplus dataset
#'
#' @description Build trajectories (trips) from the \code{palmsplus} dataset. This
#' returns a \code{sf data.frame} with \code{LINESTRING} geometry. Three columns
#' are returned by default (\code{identifier}, \code{tripnumber}, and \code{geometry}).
#' Additional columns can be specified with \code{\link{palms_add_trajectory_field}}
#' and \code{\link{palms_add_trajectory_location}}.
#'
#' @param data The palmsplus data obtained from \code{\link{palms_build_palmsplus}}.
#' @param trajectory_fields trajectory_fields
#' @param trajectory_locations trajectory_locations
#'
#' @return A table of individual trips represented as \code{LINESTRING} geometry.
#'
#' @import dplyr
#' @import sf
#' @importFrom rlang parse_expr
#' @importFrom stats setNames
#'
#' @export
# Code modified from https://thets.github.io/palmsplusr/
hbt_build_trajectories <- function(data = NULL, trajectory_fields = NULL, trajectory_locations = NULL) {
  name = after_conversion = tripnumber = NULL
  
  args <- trajectory_fields %>% filter(after_conversion == FALSE)
  args_after <- trajectory_fields %>% filter(after_conversion == TRUE)
  
  args <- setNames(args$formula, args$name) %>% lapply(parse_expr)
  args_after <- setNames(args_after$formula, args_after$name) %>% lapply(parse_expr)
  
  if (length(trajectory_locations) > 0) {
    args_locations <- setNames(paste0("first(", trajectory_locations$start_criteria,
                                      ") & last(", trajectory_locations$end_criteria, ")"),
                               trajectory_locations$name) %>% lapply(parse_expr)
    args_locations = args_locations[order(names(args_locations))]
    args <- c(args, args_locations)
  }
  
  # Build data object
  data <- data %>%
    filter(tripnumber > 0) %>%
    group_by(identifier, tripnumber) %>%
    summarise(!!!args, do_union = FALSE) %>%
    st_cast("LINESTRING") %>%
    mutate(!!!args_after) %>%
    ungroup() %>%
    mutate_if(is.logical, as.integer)
  
  return(data)
}





