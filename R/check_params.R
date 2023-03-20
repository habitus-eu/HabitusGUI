#' check_params
#'
#' @param params Parameter object to be checked
#' @param tool Tool name for which parameters are checked
#' @return Data.frame with problematic parameter values and corresponding error
#' @export
#' 
check_params = function(params = c(), tool = c()) {
  N = nrow(params)
  blocked_params = data.frame(name = character(N), error = character(N))
  rowNames = rownames(params)
  
  # Check parameters which value should be numeric and inside a continues range.
  numi = which(params$class == "double" | params$class == "integer")
  cnt = 1
  if (length(numi) > 0) {
    for (i in numi) {
      try(expr = {
        num_value = suppressWarnings(
          as.numeric(unlist(lapply(strsplit(x = as.character(params$value[i]), "[(]|[)]|[,]|c"), function(x){x[!x == ""]})))
        )
      }, silent = TRUE)
      test_na = any(is.na(num_value))
      if (test_na == TRUE) {
        blocked_params$name[cnt] = rowNames[i]
        blocked_params$error[cnt] = "is not numeric"
        cnt = cnt + 1
      } else {
        if (length(num_value) > 1) {
          if (tool == "GGIR") {
            nc = nchar(params$value[i])
            if (substr(params$value[i], 1, 2) != "c(" | substr(params$value[i], nc, nc) != ")") {
              blocked_params$name[cnt] = rowNames[i]
              blocked_params$error[cnt] = "numeric vector needs to start with c( and end with ), with values separated by a comma"
              cnt = cnt + 1
            }
          }
        }
      }
      if (test_na == FALSE) {
        if (params$class[i] == "integer") {
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
                                             as.numeric(params$maximum[i]), collapse = "")
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
        blocked_params$error[cnt] = paste0("is not among expected values: ", 
                                           gsub(pattern = ";", replacement = ", ", x = params$set[i]),
                                           collapse = "")
        cnt = cnt + 1
      }
    }
  }
  # Check parameters which value should be timezone
  seti = which(params$class == "timezone")
  if (length(seti) > 0) {
    for (i in seti) {
      if (params$value[i] %in% OlsonNames() ==  FALSE) {
        blocked_params$name[cnt] = rowNames[i]
        blocked_params$error[cnt] = "is not an expected (Olson) timezone name. You may want to check spelling."
        cnt = cnt + 1
      }
    }
  }
  
  # Create messages
  if (cnt <= N) {
    blocked_params = blocked_params[-c(cnt:N),]
  }
  block_params = blocked_params[order(blocked_params$name),]
  
  green_message = "Configuration file has succesfully passed all formatting checks"
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