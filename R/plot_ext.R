#' Plot light extinction parameters
#'
#' Plots e-folding depths of visible and non-visible light.
#'
#' @param file value file; File path to GOTM meteorlogical input file.
#' @param header logical; indicating whether the file contains the names of the variables as its first line. Defaults to FALSE.
#' @param sep character; field separator character. Values on each line of the file are separated by this character. Defaults to tab
#' @param start string; Start date of range of data to be plotted.
#' @param stop string; Stop date of range of data to be plotted.
#' @param main string; an overall title for the plot
#' @param ylab string; a title for the y axis
#' @param tz Timezone string to be supplied to as.POSIXct. Defaults to 'UTC'. This often can be left to the default unless timezone support is specifically required.
#' @param type string; what type of plot should be drawn. Uses graphics::plot names. Defaults to 'line'.
#' @param pch string; Point character to be plotted. Defaults to '.'.
#' @return Dataframe with Date time and values in wide format
#' @importFrom utils read.delim
#' @import graphics
#' @export
plot_ext <- function(file, header =F, sep = "\t", start = NULL, stop = NULL,
                     tz ='UTC', main = '', ylab = '', type = 'l', pch = '.'){
  inp = read.delim(file, header = header, sep =sep)
  if(sep == ' '){
    inp$DateTime = paste(inp[,1], inp[,2])
    inp = inp[,c(6,3:5)]
  }else{
    colnames(inp)[1] <- 'DateTime'
  }
  inp$DateTime <- as.POSIXct(inp$DateTime, tz = tz)
  if(!is.null(start) & !is.null(stop)){
    inp = inp[(inp$DateTime >= start & inp$DateTime < stop),]
  }
  mx = max(inp[,c(3,4)])
  mn = min(inp[,c(3,4)])
  plot(inp$DateTime, inp[,3], main = 'A, g1 and g2', ylab = 'm', type = type,
       xlab = 'Date', pch =pch, ylim = c(mx,mn), col =3)
  lines(inp$DateTime, inp[,4], col =2)
  par(new =T)
  plot(inp$DateTime, inp$V3, type = 'l', lty =2, col =1, axes = F, xlab = '',
       ylab = '', ylim = c(0,1))
  axis(4)
  legend('topright', legend = c('A', 'g1', 'g2'), col = c(1,3,2), lty = c(2,1,1))
}
