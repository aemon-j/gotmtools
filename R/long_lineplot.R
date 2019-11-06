#' Create line plot from observed temperature in long format
#'
#' Plots a line plot of the temperatures with the lines coloured for each depth.
#'
#' @param data dataframe; observations loaded with load.obs
#' @param rev.depths logical; Reverse the labelling of the depths. Defaults to FALSE
#' @param main character; Title of the graph. Defaults to 'Lineplot'
#' @param ylab character; Label of the y-axis. Defaults to 'Temperature'
#' @param xlim vector; Limits for the x-axis. Defaults to range of values in the data
#' @param colourblind logical; Use colourblind friendly colours. Defaults to TRUE
#' @return Line plot of temperatures at different depths
#' @import ggplot2
#' @importFrom RColorBrewer brewer.pal
#' @importFrom grDevices colorRamp
#' @export
#'
long_lineplot <- function(data, rev.depths = FALSE, ylab = 'Temperature', main = 'Lineplot', colourblind = TRUE){
  data[,2] <- factor(data[,2])
  ndep = length(unique(data[,2]))
  if(colourblind){
    dramp <- colorRampPalette(RColorBrewer::brewer.pal(8, 'Dark2'))
  }
  if(rev.depths == TRUE){
    data[,2] <- factor(data[,2], rev(levels(data[,2])))
  }
  p1 <- ggplot(data, aes_string(names(data)[1], names(data)[3], colour = names(data)[2]))+
    geom_line()+
    ggtitle(main)+
    {if(colourblind)scale_colour_manual(values = dramp(ndep))}+
    guides(colour = guide_legend(override.aes = list(size=4, alpha = 1, shape = 0), title = 'Depths'))+
    ylab(ylab)+
    theme_bw()
  return(p1)
}

