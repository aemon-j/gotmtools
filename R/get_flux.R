#' Extract fluxes from NetCDF file
#'
#' Extracts fluxes from the netCDF output file; Sensible heat flux (Qe), latent heat flux (Qh), long-wave back radation (Qb), incoming solar radiation (Qsw) and calculates the net sum of all these fluxes (Net).
#' Net = Qe + Qh + Qb + Qsw
#' Negative flux represents upward flux while positive indicated downward flux.
#'
#' @param ncdf filepath; Name of the netCDF file to extract variable
#' @param agg.dly logical; Aggregate the fluxes to a daily timestep.
#' @return dataframe with the DateTime and heat fluxes in each columns.
#' @importFrom ncdf4 nc_open
#' @importFrom ncdf4 nc_close
#' @importFrom ncdf4 ncvar_get
#' @importFrom ncdf4 ncatt_get
#' @export
get_flux <- function(ncdf, agg.dly = FALSE){

  fid = nc_open(ncdf)
  tim = ncvar_get(fid, 'time')
  tunits = ncatt_get(fid,'time')
  #Extract time and formate Date
  lnam = tunits$long_name
  tustr <- strsplit(tunits$units, " ")
  step = tustr[[1]][1]
  tdstr <- strsplit(unlist(tustr)[3], "-")
  tmonth <- as.integer(unlist(tdstr)[2])
  tday <- as.integer(unlist(tdstr)[3])
  tyear <- as.integer(unlist(tdstr)[1])
  origin = as.POSIXct(paste0(tyear,'-',tmonth,'-',tday), format = '%Y-%m-%d', tz = 'UTC')
  time = as.POSIXct(tim, origin = origin, tz = 'UTC')

  qe = ncvar_get(fid, 'qh')
  qh = ncvar_get(fid, 'qe')
  qb = ncvar_get(fid, 'qb')
  qsw = ncvar_get(fid, 'I_0') #Albedo is included in this calculation
  net = qe + qh + qb + qsw
  nc_close(fid)

  #Extract time and formate Date
  df <- data.frame(DateTime = time, Qe = qe, Qh = qh, Qb = qb, Qsw = qsw, Net = net)
  if(agg.dly == TRUE){
    df = aggregate(list(Qe = df$Qe, Qh = df$Qh, Qb = df$Qb, Qsw = df$Qsw, Net = df$Net), by = list(DateTime = cut(df$DateTime, '1 day')), mean)
    df$DateTime <- as.POSIXct(df$DateTime, tz = 'UTC')
  }
  return(df)
}
