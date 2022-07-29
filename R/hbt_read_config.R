#' Read palmsplusr config file
#'
#' @param path Path to config file
#'
#' @return dataframe
#'
#' @importFrom readr read_csv
#'
#' @export
hbt_read_config <- function(path) {

  if (file.exists(path)) {
    df <- read_csv(path, show_col_types = FALSE)
  } else {
    stop("No config file found at ", path)
  }


  # TODO sanity checks

  return(df)
}
