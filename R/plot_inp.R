#' Plot extra inputs
#'
#' Plots additional variables used for input into GOTM i.e. SWR, precip etc.
#'
#' @param file value file; File path to GOTM meteorlogical input file.
#' @param header logical; indicating whether the file contains the names of the variables as its first line. Defaults to FALSE.
#' @param sep character; field separator character. Values on each line of the file are separated by this character. Defaults to tab
#' @param start string; Start date of range of data to be plotted.
#' @param stop string; Stop date of range of data to be plotted.
#' @param ncol integer; Index of which column to use.
#' @param main string; an overall title for the plot
#' @param ylab string; a title for the y axis
#' @param tz Timezone string to be supplied to as.POSIXct. Defaults to 'UTC'. This often can be left to the default unless timezone support is specifically required.
#' @param pch string; Point character to be plotted. Defaults to '.'.
#' @return Dataframe with Date time and values in wide format
#' @importFrom utils read.delim
#' @export
plot_inp <- function(file, header =F, sep = "\t", start = NULL, stop = NULL, ncol =2,
                     tz ='UTC', main = '', ylab = '', type = 'p', pch = '.'){
  inp = read.delim(file, header = header, sep =sep)
  if(sep == ' '){
    inp$DateTime = paste(inp[,1], inp[,2])
  }else{
    colnames(inp)[1] <- 'DateTime'
  }
  inp$DateTime <- as.POSIXct(inp$DateTime, tz = tz)
  if(!is.null(start) & !is.null(stop)){
    inp = inp[(inp$DateTime >= start & inp$DateTime < stop),]
  }
  plot(inp$DateTime, inp[,ncol], main = main, ylab = ylab, type = type,
       xlab = 'Date', pch =pch)
}
