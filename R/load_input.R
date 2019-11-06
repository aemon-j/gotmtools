#' Load inflow input files
#'
#' Load observations which are used for calibration, for plotting in a heatmap
#' @param input_file observation file; File path to ACPy observation file containing the time, depths and values of observations
#' @param header logical; file contains the names of the variables as its first line. Defaults to FALSE
#' @param sep character; the delimiter in the file. Defaults to tab.
#' @param tz Assigns timezone to the DateTime column. Defaults to 'UTC'
#' @return Dataframe of observation data for ACPy
#' @export
load_input <- function(input_file, header = F,sep = "\t", tz = 'UTC'){
  if(sep == ' '){
    inp = read.delim(input_file, header = header, sep = sep)
    inp$date = paste(inp[,1], inp[,2])
    inp = inp[,c(ncol(inp),3:(ncol(inp) -1))]
    colnames(inp) = c('date', paste0('V',(1:(ncol(inp) -1))))
  }else{
    inp = read.delim(input_file, header = header, sep = sep)
  }
  inp[,1] = as.POSIXct(as.character(inp[,1]),format = '%Y-%m-%d %H:%M:%S', tz = tz)
  return(inp)
}
