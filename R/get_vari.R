#' Extract variables from NetCDF file
#'
#' Extracts a selected parameter from the netCDF file and formats it into a dataframe.
#'
#' @param ncdf filepath; Name of the netCDF file to extract variable
#' @param var character; Name of the variable to be extracted. Must match short name in netCDF file
#' @param incl_time boolean; Add time to the first column in the dataframe. Defaults to TRUE
#' @param print logical; Print the name and units of the variable extracted, defaults to TRUE
#' If coordinates are not in ncdf use print = FALSE
#' @return dataframe in the same format as the observation file with the surface in the top column and the bottom in the last column.
#' @importFrom ncdf4 nc_open
#' @importFrom ncdf4 nc_close
#' @importFrom ncdf4 ncvar_get
#' @importFrom ncdf4 ncatt_get
#' @examples
#' sim_folder <- system.file('extdata', package = 'GOTMr')
#' run_gotm(sim_folder)
#' out <- file.path(sim_folder, 'output', 'output.nc')
#' wtemp <- get_vari(ncdf = out, var = 'temp')
#' z <- get_vari(ncdf = out, var = 'z')
#' @export
get_vari <- function(ncdf, var, incl_time = TRUE, print = TRUE){
  fid = nc_open(ncdf)
  if(incl_time){
    tim = ncvar_get(fid, "time")
    tunits = ncatt_get(fid, "time")
    lnam = tunits$long_name
    tustr <- strsplit(tunits$units, " ")
    step = tustr[[1]][1]
    tdstr <- strsplit(unlist(tustr)[3], "-")
    tmonth <- as.integer(unlist(tdstr)[2])
    tday <- as.integer(unlist(tdstr)[3])
    tyear <- as.integer(unlist(tdstr)[1])
    tdstr <- strsplit(unlist(tustr)[4], ":")
    thour <- as.integer(unlist(tdstr)[1])
    tmin <- as.integer(unlist(tdstr)[2])
    origin <- as.POSIXct(paste0(tyear, "-", tmonth,
                                "-", tday, ' ', thour, ':', tmin),
                         format = "%Y-%m-%d %H:%M", tz = "UTC")
    if( step == "hours") {
      tim <- tim * 60 * 60
    }
    if( step == "minutes") {
      tim <- tim * 60
    }
    time = as.POSIXct(tim, origin = origin, tz = "UTC")
  }
  var1 = ncvar_get(fid, var)
  tunits = ncatt_get(fid, var)
  nc_close(fid)
  lnam = tunits$long_name
  dims = dim(var1)
  if (length(dims) == 2) {
    flag <- which(dim(var1) == length(time))
    if(flag == 2){
      var1 = as.data.frame(t(var1))
    }else{
      var1 <- as.data.frame(var1)
    }
    var1 <- var1[, ncol(var1):1]
    if(incl_time){
      var1$Datetime <- time
      var1 <- var1[, c(ncol(var1), 1:(ncol(var1) - 1))]
    }
  }else if (length(dims) == 1) {
    if(incl_time){
      var1 <- data.frame(time, var1)
    }
  }
  if (print == TRUE) {
    mat = matrix(data = c(var, lnam, tunits$units, tunits$coordinates),
                 dimnames = list(c("short_name", "long_name",
                                   "units", "dimensions"), c()))
    message("Extracted ", var, " from ", ncdf)
    print(mat)
  }
  return(var1)
}

