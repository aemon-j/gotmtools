#' View NetCDF file
#'
#' Prints the data from a netCDF file for viewing.
#' @param ncdf filepath; To the location of the NetCDF file to be viewed.
#' @return Prints text to the console with details from the NetCDF file.
#' @importFrom ncdf4 nc_open
#' @importFrom ncdf4 nc_close
#' @export
view_nc <- function(ncdf){
  fid = nc_open(ncdf)
  print(fid)
  nc_close(fid)
}
