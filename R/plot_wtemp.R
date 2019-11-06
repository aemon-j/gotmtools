#' Plot wtemp from netcdf file
#'
#' Extracts modelled water temperature from netcdf file and plots it using long_heatmap function
#'
#' @param file file; File path to netcdf file
#' @param ... other arguments to be passed to long_heatmap
#' @return ggplot object; filled point plot of water temperature
#' @export
plot_wtemp <- function(file, ...){
  wtemp <- get_vari(ncdf = file, var = 'temp')
  z = get_vari(ncdf = file, var = 'z')
  tmp = wide2long(wtemp,z)
  p1 = long_heatmap(tmp, ...)
  return(p1)
}
