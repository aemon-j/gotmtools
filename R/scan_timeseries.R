#' Extract start and stop dates from a GOTM input file
#'
#' Extracts start and stop dates from a GOTM input file
#'
#' @param file file; File path to ACPy observation file containing the time, depths and values of observations
#' @param header logical; file contains the names of the variables as its first line. Defaults to FALSE
#' @param sep character; the delimiter in the file. Defaults to tab
#' @return vector with start and stop dates as a character
#' @importFrom utils read.delim
#' @export
scan_timeseries <- function(file, header = F, sep = "\t"){
  dat = read.delim(file, header = header, sep = sep)
  if(sep == "\t"){
    dat$DateTime <- as.POSIXct(dat[,1], tz = 'UTC')
  }else if(sep == ' '){
    dat$DateTime <- as.POSIXct(paste(dat[,1], dat[,2]), tz = 'UTC')
  }
  start = as.character(format(range(dat$DateTime)[1], '%Y-%m-%d %H:%M:%S'))
  stop = as.character(format(range(dat$DateTime)[2], '%Y-%m-%d %H:%M:%S'))
  df = c(start, stop)
  return(df)
}
