#' Plot hypsograph data
#'
#' Plots hysograph curve for the lake
#'
#' @param hypsograph_file value file; File path to GOTM hypsograph input file.
#' @param header logical; indicating whether the file contains the names of the variables as its first line. Defaults to FALSE.
#' @param sep character; field separator character. Values on each line of the file are separated by this character. Defaults to tab
#' @param title character; Title for the graph. Defaults to "Hypsograph"
#' @return Plot of hypsograph
#' @importFrom ggplot2 ggplot
#' @importFrom utils read.delim
#' @export
plot_hypso <- function(hypsograph_file,header =F, sep = "\t", title = 'Hypsograph'){
  hyp <- read.delim(hypsograph_file, header = F,sep = sep, stringsAsFactors = F)
  if(is.character(hyp[,1])){
    hyp[,1] <- as.numeric(hyp[,1])
  }
  p1 <- ggplot(hyp[-1,], aes(V2,V1))+
    geom_line()+
    geom_point()+
    ggtitle(title)+
    xlab('Area (m2)')+
    ylab('Depth (m)')+
    theme_bw()
  return(p1)
}
