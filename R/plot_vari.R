#' Plot any variable from netcdf file
#'
#' Extracts modelled water temperature from netcdf file and plots it using long_heatmap function
#'
#' @param ncdf filepath; to the netCDF file to extract variable
#' @param var character; Name of the variable to be extracted. Must match short name in netCDF file
#' @param ... other arguments to be passed to long_heatmap
#' @return ggplot object; filled point plot of water temperature
#' @importFrom glmtools get_var
#' @examples
#' sim_folder <- system.file('extdata', package = 'GOTMr')
#' run_gotm(sim_folder)
#' out <- file.path(sim_folder, 'output', 'output.nc')
#' plot_vari(ncdf = out, var = 'temp')
#' @export
plot_vari <- function(ncdf, var, ...){
  vari <- get_vari(ncdf = ncdf, var = var)
  z = get_vari(ncdf = ncdf, var = 'z')
  tmp = wide2long(vari,z)
  p1 = long_heatmap(tmp, ...)
  return(p1)
}
