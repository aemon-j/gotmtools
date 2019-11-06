#' List variables within the NetCDF file
#'
#' Prints the short or long names of the variables within the NetCDF file
#' @param ncdf filepath; To the location of the NetCDF file to be viewed.
#' @param long logical; Print the longnames of the variables, if FALSE it will print the short names. Defaults to FALSE
#' @return Prints text to the console with the names of the variables from the NetCDF file.
#' @importFrom ncdf4 nc_open
#' @importFrom ncdf4 nc_close
#' @export
list_vars <- function(ncdf,long =F){
  fid = nc_open(ncdf)
  vars = fid$var
  if(long == T){
    nams = c()
    for(i in 1:length(vars)){
      nams = append(nams,unlist(vars[[i]]['longname']))
    }
    nams = as.vector(nams)
  }else if(long == F){
    nams = names(vars)
  }
  nc_close(fid)
  return(nams)
}
