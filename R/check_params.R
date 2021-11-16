#' check_params
#'
#' @param params Parameter object to be checked
#' @return Data.frame with problematic parameter values and corresponding error
#' @export
#' 
check_params = function(params = c()) {
  N = nrow(params)
  blocked_params = data.frame(name = character(N), error = character(N))
  rowNames = rownames(params)
  
  # Check parameters which value should be numeric and inside a continues range.
  numi = which(params$class == "num_double" | params$class == "num_integer")
  cnt = 1
  if (length(numi) > 0) {
    for (i in numi) {
      try(expr = {
        test_num = any(is.na(suppressWarnings(as.numeric(params$value[i]))))
      }, silent = TRUE)
      if (test_num == TRUE) {
        blocked_params$name[cnt] = rowNames[i]
        blocked_params$error[cnt] = "Value is not numeric"
        cnt = cnt + 1
      } 
      if (test_num == FALSE) {
        num_value = as.numeric(params$value[i])
        if (params$class[i] == "num_integer") {
          if (round(num_value) != num_value) {
            blocked_params$name[cnt] = rowNames[i]
            blocked_params$error[cnt] = "Number is not an integer"
            cnt = cnt + 1
          }
        }
        # below code accounts for both scalar and vector numerics
        if (any(num_value < as.numeric(params$minimum[i]) | 
                num_value > as.numeric(params$maximum[i]))) {
          blocked_params$name[cnt] = rowNames[i]
          blocked_params$error[cnt] = "Number is not within expected range"
          cnt = cnt + 1
        }
      }
    }
  }
  
  # Check parameters which value should be part of a set character or numeric.
  seti = which(params$class == "set")
  if (length(seti) > 0) {
    for (i in seti) {
      if (params$value[i] %in% unlist(strsplit(params$set[i], ";")) ==  FALSE) {
        blocked_params$name[cnt] = rowNames[i]
        blocked_params$error[cnt] = "Value is not part of expected set"
        cnt = cnt + 1
      }
    }
  }
  if (cnt <= N) {
    blocked_params = blocked_params[-c(cnt:N),]
  }
  block_params = blocked_params[order(blocked_params$name),]
  return(blocked_params)
}