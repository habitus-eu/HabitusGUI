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
        num_value = suppressWarnings(
          as.numeric(unlist(lapply(strsplit(x = params$value[i], "[(]|[)]|[,]|c"), function(x){x[!x == ""]})))
          )
      }, silent = TRUE)
      test_num = any(is.na(num_value))
      if (test_num == TRUE) {
        blocked_params$name[cnt] = rowNames[i]
        blocked_params$error[cnt] = "is not numeric"
        cnt = cnt + 1
      } 
      if (test_num == FALSE) {
        if (params$class[i] == "num_integer") {
          if (any(round(num_value) != num_value)) {
            blocked_params$name[cnt] = rowNames[i]
            blocked_params$error[cnt] = "is not an integer"
            cnt = cnt + 1
          }
        }
        # below code accounts for both scalar and vector numerics
        if (any(num_value < as.numeric(params$minimum[i]) | 
                num_value > as.numeric(params$maximum[i]))) {
          blocked_params$name[cnt] = rowNames[i]
          blocked_params$error[cnt] = paste0("is not within expected range: ",
                                             as.numeric(params$minimum[i]), " - ",
                                             as.numeric(params$maximum[i]), collapse="")
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
        blocked_params$error[cnt] = paste0("is not among excpected values: ", 
                                           gsub(pattern = ";", replacement = ", ", x = params$set[i]),
                                           collapse = "")
        cnt = cnt + 1
      }
    }
  }
  if (cnt <= N) {
    blocked_params = blocked_params[-c(cnt:N),]
  }
  block_params = blocked_params[order(blocked_params$name),]
  
  green_message = "Configuration file has succesfully passed all checks"
  message = c()
  if (nrow(block_params) > 0) {
    for (parError_i in 1:nrow(block_params)) {
      # Error in parameter " idloc ": 10 is not part of expected set: 1 / 2 / 3 / 4 / 5 / 6 / 7
      message = paste0(c(message, paste0("Error in parameter \" ", 
                                  block_params$name[parError_i], 
                                  " \": Value ", params$value[which(rownames(params) == block_params$name[parError_i])], " ",
                                  block_params$error[parError_i],"<br/>")), collapse = "")
    }
    green_message = c()
  }
  invisible(list(blocked_params = blocked_params, error_message = message, green_message = green_message))
}