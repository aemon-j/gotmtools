#' Load layers file
#'
#' Load layers file as a dataframe for use in plotting heatmap of the corresponding 3-d variable
#' @param z.file depths file; File path to GOTM output file containing the depths which corresponds to the value file
#' @param tz Timezone string to be supplied to as.POSIXct. Defaults to 'UTC'. This often can be left to the default unless timezone support is specifically required.
#' @return Dataframe with depths corresponding to values loaded via load.3d
#' @export
load_depths <- function(z.file, tz = 'UTC'){
  layers = read.delim(z.file, skip = 9, header = FALSE)
  layers[,1] = as.POSIXct(layers[,1], tz = tz)
  layers = layers[,c(1,ncol(layers):2)]
}
