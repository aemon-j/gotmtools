#' Calculate SWR using calculation within GOTM
#'
#' Calculates incoming SWR using cloud cover, time, latitude and longitude. Calculations taken from the GOTM code. See https://github.com/gotm-model/code Note: For GOTM all inputs have to be in UTC so be sure to switch back to local timezone if working with subdaily data.
#'
#' @param time vector; In POSIXct format
#' @param lat numeric; Latitude in decimal degrees
#' @param lon numeric; Longitude in decimal degrees
#' @param cloud vector; Cloud cover values [0-1] which correspond to the times supplied
#' @param daily2hour logical; Indicate whether you are converting daily cloud cover data to hourly SWR or hourly cloud cover. Default = FALSE
#' @return If daily2hour = FALSE, then a vector of SWR values are returned corresponding to input time vector. If TRUE, a data.frame with the DateTime and SWR are returned.
#' @export
calc_swr <- function(time, lat, lon, cloud, daily2hour = F){

  if(daily2hour == T){
    time = seq.POSIXt(from = time[1], to = (time[length(time)] + 23*60*60), by = '1 hour')
    cloud = rep(cloud,each =24)
  }

  yday <- lubridate::yday(time)  #extracts the julian day
  hh <- lubridate::hour(time) #Extracts the hour

  deg2rad=pi/180
  rad2deg=180./pi

  solar=1350.
  eclips=23.439*deg2rad
  tau=0.7
  aozone=0.09

  ######### From GOTM solar_zenith_angle.F90
  rlon = deg2rad*lon
  rlat = deg2rad*lat

  yrdays=365.25

  th0 = 2.*pi*yday/yrdays
  th02 = 2.*th0
  th03 = 3.*th0
  #sun declination :
  sundec = 0.006918 - 0.399912*cos(th0) + 0.070257*sin(th0) - 0.006758*cos(th02) + 0.000907*sin(th02) - 0.002697*cos(th03) + 0.001480*sin(th03)
  #sun hour angle :
  thsun = (hh-12.)*15.*deg2rad + rlon

  #cosine of the solar zenith angle :
  coszen =sin(rlat)*sin(sundec)+cos(rlat)*cos(sundec)*cos(thsun)
  coszen[coszen < 0] <- 0
  solar_zenith_angle = rad2deg*acos(coszen)

  ####### From GOTM short_wave_radiation.F90
  #from now on everything in radians
  coszen = cos(deg2rad*solar_zenith_angle)
  qatten = rep(NA, length(coszen))
  coszen[coszen <= 0] <- 0
  qatten[coszen <= 0] <- 0
  qatten[coszen > 0] <- tau^(1/coszen)[coszen > 0]

  qzer  = coszen * solar
  qdir  = qzer * qatten
  qdiff = ((1-aozone)*qzer - qdir) * 0.5
  qtot  =  qdir + qdiff

  rlon = deg2rad*lon
  rlat = deg2rad*lat
  yrdays=365.
  eqnx = (yday-81.)/yrdays*2.*pi
  #sin of the solar noon altitude in radians :
  sunbet=sin(rlat)*sin(eclips*sin(eqnx))+cos(rlat)*cos(eclips*sin(eqnx))
  #solar noon altitude in degrees :
  sunbet = asin(sunbet)*rad2deg

  qshort  = qtot*(1-0.62*cloud + .0019*sunbet) #From GOTM short_wave_radiation.F90
  qshort[which(cloud <0.3)] <- qtot[which(cloud <0.3)]
  qshort[solar_zenith_angle == 90] <- 0 #Assign Zero when the sun is down.

  if(daily2hour == T){
    df = data.frame(DateTime = time, SWR = qshort)
    return(df)
  }else{
    return(qshort)
  }
}
