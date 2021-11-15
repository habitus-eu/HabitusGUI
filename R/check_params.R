#' check_params
#'
#' @param params Parameter object to be checked
#' @return Data.frame with problematic parameter values and corresponding error
#' @export
#' 
check_params = function(params = c()) {
  N = nrow(params)
  blocked_params = data.frame(name = character(N), error = character(N))
  numi = which(params$class == "num")
  rowNames = rownames(params)
  cnt = 1
  if (length(numi) > 0) {
    for (i in 1:length(numi)) {
      try(expr = {
        test_num = any(is.na(suppressWarnings(as.numeric(params$value[numi[i]]))))
      }, silent = TRUE)
      if (test_num == TRUE) {
        blocked_params$name[cnt] = rowNames[i]
        blocked_params$error[cnt] = "Value is not numeric"
        cnt = cnt + 1
      } 
      if (test_num == FALSE) {
        num_value = as.numeric(params$value[numi[i]])
        # below code accounts for both scalar and vector numerics
        if (any(num_value < as.numeric(params$minimum[i]) | 
                num_value > as.numeric(params$maximum[i]))) {
          blocked_params$name[cnt] = rowNames[i]
          blocked_params$error[cnt] = "Value not within expected range"
          cnt = cnt + 1
        }
      }
    }
  }
  if (cnt <= N) {
    blocked_params = blocked_params[-c(cnt:N),]
  }
  return(blocked_params)
}