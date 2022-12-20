
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
#' @param palmsplus_domains ...
#' @param palmsplus_fields ...
#' @param loca Nested list with location information
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
# Code modified from https://thets.github.io/palmsplusr/
hbt_build_days <- function(data = NULL, verbose = TRUE, 
                           palmsplus_domains = NULL,
                           palmsplus_fields = NULL,
                           loca = NULL,
                           participant_basis = NULL) {
  
  # Note:
  # home, school, home_nbh, school_nbh (or similar) need to be present, 
  # because the functions that are passed on assume that they exist
  # So, now we need to create those objects from object loca
  Nlocations = length(loca)
  for (i in 1:Nlocations) {
    txt = paste0(names(loca[[i]])[1], " = loca[[i]][[1]]")
    eval(parse(text = txt))
  }
  
  duration = datetime = name = domain_field = NULL
  
  domain_fields <- palmsplus_domains %>% filter(domain_field == TRUE)
  domain_names <- domain_fields %>% pull(name)
  
  if (is.null(domain_names)) {
    domain_names <- "total"
  } else {
    domain_names <- c("total", domain_names)
  }
  domain_args <- setNames("1", "total") %>% lapply(parse_expr)
  domain_args <- c(domain_args, setNames(domain_fields[[2]], domain_fields[[1]]) %>%
                     lapply(parse_expr))
  
  data <- data %>%
    mutate(!!! domain_args) %>%
    mutate_if(is.logical, as.integer)
  
  fields <- palmsplus_fields %>% filter(domain_field == TRUE) %>% pull(name)
  data <- data %>%
    st_set_geometry(NULL) %>%
    dplyr::select(identifier, datetime, domain_names, all_of(fields)) %>%
    mutate(duration = 1) %>%
    mutate_at(vars(-identifier,-datetime), ~ . * palms_epoch(data) / 60) %>%
    group_by(identifier, date = as.Date(datetime)) %>%
    dplyr::select(-datetime)

  x <- list()
  for (i in domain_names) {
    x[[i]] <- data %>%
      filter(UQ(as.name(i)) > 0) %>%
      dplyr::select(-one_of(domain_names), duration) %>%
      summarise_all(~ sum(.)) %>%
      ungroup() %>%
      rename_at(vars(-identifier, -date), ~ paste0(i, "_", .))
  }
  
  result <- x %>%
    reduce(left_join, by = c("identifier" = "identifier", "date" = "date"))
  return(result)
}
