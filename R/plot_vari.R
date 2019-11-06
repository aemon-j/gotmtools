#' Plot any variable from netcdf file
#'
#' Extracts modelled water temperature from netcdf file and plots it using long_heatmap function
#'
#' @param file file; File path to netcdf file
#' @param var character; Name of the variable to be extracted. Must match short name in netCDF file
#' @param ... other arguments to be passed to long_heatmap
#' @return ggplot object; filled point plot of water temperature
#' @importFrom glmtools get_var
#' @export
plot_vari <- function(file, var, ...){
  vari <- get_vari(ncdf = file, var = var)
  z = get_vari(ncdf = file, var = 'z')
  tmp = wide2long(vari,z)
  p1 = long_heatmap(tmp, ...)
  return(p1)
}
