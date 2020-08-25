#' Plot any variable from netcdf file
#'
#' Extracts modelled water temperature from netcdf file and plots it using long_heatmap function
#'
#' @param ncdf filepath; to the netCDF file to extract variable
#' @param var character; Name of the variable to be extracted. Must match short name in netCDF file
#' @param incl_time boolean; Add time to the first column in the dataframe. Defaults to TRUE
#' @param ... other arguments to be passed to long_heatmap
#' @return ggplot object; filled point plot of water temperature
#' @examples
#' sim_folder <- system.file('extdata', package = 'GOTMr')
#' run_gotm(sim_folder)
#' out <- file.path(sim_folder, 'output', 'output.nc')
#' plot_vari(ncdf = out, var = 'temp')
#' @export
plot_vari <- function(ncdf, var, incl_time = TRUE, ...){
  vari <- get_vari(ncdf = ncdf, var = var)
  z = get_vari(ncdf = ncdf, var = "z", incl_time = incl_time)
  flag = dim(z)
  if(length(flag) == 1){
    deps = rep(z, nrow(vari), ncol = length(z), byrow = TRUE)
    mat = matrix(rep(z, nrow(vari)), nrow = nrow(vari), ncol = length(z), byrow = TRUE)
    z <- vari
    z[,-1] <- mat
  }
  tmp = wide2long(vari, z)
  p1 = long_heatmap(tmp, ...)
  return(p1)
}
