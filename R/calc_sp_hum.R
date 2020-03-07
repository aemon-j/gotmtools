#' Calculate specific humidity
#'
#' Calculates specific humidity using air temperature and relative humidity or dewpoint temperature or wet bulb temperature. Using formula from GOTM source code: https://github.com/gotm-model/code/blob/master/src/airsea/humidity.F90
#'
#' @param airt vector; air temperature values (Celsius)
#' @param airp vector; surface air pressure values (Pa)
#' @param hum vector; humidity values (either relative humidity \% [0-100] or dewpoint temperature [degC] or wet bulb temperature). Method
#' @param method character; Determine humidity values supplied to `hum`, either relative humidity 'rhum' or dewpoint temperature 'dewt' or wet bulb temperature 'wet_bulb'
#' @examples
#'  met_file <- system.file('extdaairt/met_file.dat', package = 'GOTMr')
#'  met <- read.delim(met_file)
#'  sp_hum <- calc_sp_hum(airt = met$AirT, airp = (met$MSLP*100), hum = met$DewT, method = 'dewt')
#'  plot(sp_hum)
#' @return vector of specific humidity in kg/kg
#' @export
calc_sp_hum <- function(airt, airp, hum, method){

  a1 <- 6.107799961
  a2 <- 4.436518521e-1
  a3 <- 1.428945805e-2
  a4 <- 2.650648471e-4
  a5 <- 3.031240396e-6
  a6 <- 2.034080948e-8
  a7 <- 6.136820929e-11
  const06 <- 0.62198

  if(method == 'rhum'){
    rh <- 0.01 * hum
    #saturation vapor pressure at that air temperature
    ea <- a1 +airt*(a2+airt*(a3+airt*(a4+airt*(a5+airt*(a6+airt*a7)))))
    ea <- ea * 100.0 # Conversion millibar --> Pascal
    #get actual vapor pressure
    ea <- rh * ea
  }else if(method == 'dewt'){
    dew <- hum
    ea <- a1 +dew*(a2+dew*(a3+dew*(a4+dew*(a5+dew*(a6+dew*a7)))))
    ea <- ea * 100.0 # Conversion millibar --> Pascal
  }else if(method == 'wet_bulb'){
    twet <- hum
    ea <- a1 +twet*(a2+twet*(a3+twet*(a4+twet*(a5+twet*(a6+twet*a7)))))
    ea <- ea * 100.0 # Conversion millibar --> Pascal
    #actual vapor pressure
    ea <- ea - 6.6e-4*(1+1.15e-3*twet)*airp*(airt-twet)
  }
  #convert to specific humidity kg/kg
  qa <- const06*ea/(airp-0.377*ea)
  return(qa)
}
