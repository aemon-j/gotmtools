#' Extract 1-D variable from NetCDF file
#'
#' Extracts a selected 1-D parameter from the netCDF file.
#'
#' @param ncdf filepath; Name of the netCDF file to extract variable
#' @param var character; Name of the variable to be extracted. Must match
#' @return vector; 1D object
#' @importFrom ncdf4 nc_open
#' @importFrom ncdf4 nc_close
#' @export
get_1d <- function(ncdf, var){
  fid = nc_open(ncdf)
  get = ncvar_get(fid, var)
  nc_close(fid)
  return(get)
}
