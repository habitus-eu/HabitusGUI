#' GGIRshiny
#'
#' @param inputdir Path to input directory
#' @param outputdir Path to output directory
#' @param configfile Configfile path
#' @param sleepdiary Path to sleep diary
#' @param desiredtz Desired tz databasename
#' @return no object is returned, only a new file is created in the output directory
#' @import GGIR
#' @export

GGIRshiny = function(inputdir, outputdir, configfile=c(), sleepdiary=c(), desiredtz=c()) {
  print("GGIRshiny a")
  if (is.null(sleepdiary)) sleepdiary = c()
  if (length(desiredtz) > 0) { # only in scenario when config file is not available
    print("GGIRshiny b")
    GGIR::g.shell.GGIR(datadir=inputdir, outputdir=outputdir,
                       loglocation=sleepdiary, desiredtz=desiredtz)
  } else { # if configfile then use desiredtz from configfile
    print("GGIRshiny c")
    print(sleepdiary)
    print(configfile)
    print(inputdir)
    print(outputdir)
    
    GGIR::g.shell.GGIR(datadir=inputdir, outputdir=outputdir, configfile = configfile)
    # ,
    #                    loglocation=sleepdiary)
  }
}