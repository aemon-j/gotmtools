#' Calculate cloud cover
#'
#' Calculate cloud cover using latitude, air temperature, relative humidity (or dewpoint temperature) and short wave radiation using the calculations from Martin and McCutcheon (1999).
#'
#' @param date vector; Dates in as.POSixct class
#' @param airt vector; Air temperature values which correspond to the vector of dates
#' @param relh vector; Relative humidity values which correspond to the vector of dates
#' @param dewt vector; Dewpoint temperature values which correspond to the vector of dates. Used instead of relative humidity
#' @param swr vector; Short-wave radiation values which correspond to the vector of dates
#' @param lat numeric; Latitude position (in decimal)
#' @param lon numeric; Longitude position (in decimal)
#' @param elev numeric; elevation in metres above sea level
#' @param daily deprecated; logical; Is the data on a daily timestep. Defaults to FALSE
#' @return vector of cloud cover values which correspond to the vector of dates supplied
#' @examples
#'  met_file <- system.file('extdata/met_file.dat', package = 'GOTMr')
#'  swr_file <- system.file('extdata/swr_input_file.dat', package = 'GOTMr')
#'  met <- read.delim(met_file)
#'  met[,1] <- as.POSIXct(met[,1], tz = 'UTC')
#'  swr <- read.delim(swr_file)
#'  swr[,1] <- as.POSIXct(swr[,1], tz = 'UTC')
#'  met <- merge(met, swr, by = 1)
#'  cc <- calc_cc(date = met[,1], airt = met$AirT, dewt = met$DewT, swr = met$SWR, lat = 53, lon = -9.5, elev = 14, daily = F)
#'  plot(cc)
#' @importFrom stats aggregate
#' @importFrom zoo na.approx
#' @export

calc_cc <- function(date, airt, relh = NULL, dewt = NULL, swr, lat, lon, elev, daily = F){
  orig_date = date
  timestep = difftime(orig_date[2], orig_date[1], units = "secs")
  
  # If the time step is 24 hours or more, create artificial hourly time steps
  if(timestep >= as.difftime(24, units = "hours")){
    date = seq.POSIXt(from = date[1], to = (date[length(date)] + timestep - 1 * 60 * 60), by = '1 hour')
  }
  
  yday <- yday(date)
  hour <- hour(date)
  hour[hour == 0] <- 24

  std.mer = seq(-90,90, 15)
  Lsm = std.mer[which.min(abs(lon - std.mer))] # Local standard meridian (degrees)

  Hsc = 1390 # Solar constant (W/m2)
  cd = 0.06 # Dust coefficient
  Rg = 0.045 # Reflectivity of the ground - extended mixed forest


  theta = lat*pi/180 # Latitude in radians

  r = 1 + 0.017 * cos((2*pi/365)*(186-yday)) # Relative earth-sun distance

  d = 23.45 * pi/180 * cos((2*pi/365)*(172-yday)) # Declination of the sun

  dts = (1/15) * (Lsm-lon) # Fraction of 15-degree increment Llm is east of Lsm
  value = (sin(theta)*sin(d))
  value = value/(cos(theta)*cos(d))
  tss = (12/pi) * acos(-value) + dts + 12 # Time of sunset
  tsu = -tss + (2 * dts) + 24 # Time of sunrise

  gamma = rep(0, length(tss)) # Correction factor
  dum = which(hour>tsu & hour<tss)
  gamma[dum] = 1

  #Calculate Hb and Htheta
  dum1 = which(hour <=12 )
  dum2 = which(hour > 12 )
  hb1  = pi/12*(hour-1-dts)
  hb1[dum1] = hb1[dum1]+pi
  hb1[dum2] = hb1[dum2]-pi
  hb  = hb1
  dum3 = which(hb1 > 2*pi)
  hb[dum3] = hb[dum3] - 2 * pi
  dum4 = which(hb1 < 0)
  hb[dum4] = hb[dum4] + 2 * pi
  #rm(c(dum3, dum4))
  he1  = pi/12*(hour-dts)
  he1[dum1] = he1[dum1]+pi
  he1[dum2] = he1[dum2]-pi
  he  = he1
  dum3 = which(he1 > 2*pi)
  he[dum3] = he[dum3] - 2*pi
  dum4 = which(he1 < 0)
  he[dum4] = he[dum4] + 2*pi
  #clear dum1 dum2 dum3 dum4

  Ho = Hsc/(r^2)*(sin(theta)*sin(d)+12/pi*cos(theta)*cos(d)*(sin(he)-sin(hb)))*gamma

  # Radiation scattering and absorption #####################################

  w = (he+hb)/2 # Hour angle
  alpha1 = abs(sin(theta)*sin(d)+cos(theta)*cos(d)*cos(w))
  alpha = atan(alpha1/sqrt(1-alpha1^2)) # Solar altitude

  theta_am1 = ((288-0.0065*elev)/288)^5.256
  theta_am2 = sin(alpha)+0.15*((alpha*180/pi)+3.855)^(-1.253)
  theta_am = theta_am1/theta_am2 # Optical air mass

  # Dewpoint temperature
  if(is.null(dewt)){
    if(any(relh <= 0.0)){
      warning("Relative humidity values of 0.0% detected; will cause error!")
    }
    
    dewt <- 243.04*(log(relh/100)+((17.625*airt)/(243.04+airt)))/(17.625-log(relh/100)-((17.625*airt)/(243.04+airt)))
  }
  if(timestep >= as.difftime(2, units = "hours")){
    dewt = rep(dewt, each = as.numeric(difftime(orig_date[2], orig_date[1], units = "hours")))
  }

  Pwc = 0.85*exp(0.11+0.0614*dewt) # Precipitable atmospheric water content

  a2 = exp(-(0.465+0.134*Pwc)*(0.179+0.421*exp(-0.721*theta_am))*theta_am) # Atmospheric transmission coefficient after scattering and absorption
  a1 = exp(-(0.465+0.134*Pwc)*(0.129+0.171*exp(-0.88*theta_am))*theta_am)
  at = (a2+0.5*(1-a1-cd))/(1-0.5*Rg*(1-a1-cd)) # attenuation (scattering and absorption)
  #att = mean(at)

  Ho = at*Ho
  #Ho = att*Ho

  dum5 = which(Ho<0.0)
  Ho[dum5] = 1

  df = data.frame(DateTime = date,Ho = Ho)
  if(timestep >= as.difftime(2, units = "hours")){
    df = aggregate(list(Ho = df$Ho), by = list(DateTime = cut(df[,1], paste(timestep, "s"))), mean, na.rm = T)
  }
  df$swr =swr

  df$ccsim <- NA

  for(i in 1:nrow(df)){
    if(df$Ho[i] < df$swr[i]){
      df$ccsim[i] <- NaN
    }else{
      df$ccsim[i] <- sqrt((1 - (df$swr[i]/df$Ho[i]))/0.65)
    }
  }

  ccsim = df$ccsim
  ccsim[ccsim > 1] <- 1

  # Fill gaps with the mean value of the previous and posterior.
  sta = min(which(!is.nan(ccsim)))
  stp = max(which(!is.nan(ccsim)))
  ccsim[sta:stp] <- zoo::na.approx(ccsim[sta:stp])
  if(sta != 1){
    ccsim[1:sta] <- ccsim[sta]
  }
  if(stp != length(ccsim)){
    ccsim[stp:length(ccsim)] <- ccsim[stp]
  }

  return(ccsim)
}
