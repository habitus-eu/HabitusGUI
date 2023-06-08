#' Cleans multiple consecutive file separator from file path
#'
#' @param path Character with file path to be cleaned
#'
#' @return Character with cleaned file path
#' @export
#'
#' @examples cleanPath("C:/myfolder///myfile.csv")
cleanPath = function(path) {
  split_tmp = unlist(strsplit(path, .Platform$file.sep))
  empty = which(split_tmp == "")
  if (length(empty) > 0) split_tmp = split_tmp[-empty]
  newPath = paste(split_tmp, collapse = .Platform$file.sep)
  return(newPath)
}
