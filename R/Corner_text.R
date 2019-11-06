#' Add text to the corner of a plot
#'
#' Add text to a location within a plot.
#'
#' @param text text to be added to the plot as a language object
#' @param location location within the plot for the text to be placed
#' @return data
#' @importFrom graphics legend
Corner_text <- function(text, location="topleft"){
  legend(location,legend=text, bty ="n", pch=NA)
}
