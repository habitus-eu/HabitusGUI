#' Cleans multiple consecutive file separator from file path
#'
#' @param path Character with file path to be cleaned
#'
#' @return Character with cleaned file path
#' @export
#'
#' @examples cleanPath("C:/myfolder///myfile.csv")
cleanPath = function(path) {
  if (!is.character(path)) return(path)
  split_tmp = unlist(strsplit(path, .Platform$file.sep))
  empty = which(split_tmp == "")
  if (length(empty) > 0) split_tmp = split_tmp[-empty]
  newPath = paste(split_tmp, collapse = .Platform$file.sep)
  # In Linux the first character of the path can be a forward
  # slash, which needs to be kept
  firstChar = substring(path, 1, 1)
  if (firstChar == "/") {
    newPath = paste0("/", newPath)
  }
  # In both Linux and Windows ~ signs may be used as shortcut to the users home
  # directory. To avoid inconsistencies we replace them by the full path.
  # If we do not do this myApp may copy ~/config.csv to /home/user/config.csv which
  # essentially is the same file and would then result in an empty file.
  newPath = normalizePath(newPath)
  return(newPath)
}
