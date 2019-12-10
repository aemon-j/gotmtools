#' Calculate incoming Long-wave radiation
#'
#' Calculates incoming LWR using cloud cover, air temperature and relative humidity (or dewpoint temperature). Using formula from: http://www.seao2.info/_TMP/longwave.pdf
#'
#' @param cc vector; cloud cover values (Fraction [0-1])
#' @param airt vector; air temperature values (Celsius)
#' @param relh vector; relative humidity values (\% [0-100]). Used to calculate dewpoint temperaure if that is not supplied. Defaults to NULL.
#' @param dewt vector; dewpoint temperature values (Celsius). Can be used instead of relative humidity, otherise it is calculated from relative humidity and air temperature. Defaults to NULL
#' @examples
#'  met_file <- system.file('extdata/met_file.dat', package = 'GOTMr')
#'  met <- read.delim(met_file)
#'  lwr_in <- calc_in_lwr(cc = met$CC, airt = met$AirT, dewt = met$DewT)
#'  plot(lwr_in)
#' @return vector of incoming long-wave radiation values
#' @export

calc_in_lwr <- function(cc, airt, relh = NULL, dewt = NULL){
  if(is.null(dewt)){
    dewt <- 243.04*(log(relh/100)+((17.625*airt)/(243.04+airt)))/(17.625-log(relh/100)-((17.625*airt)/(243.04+airt)))
  }
  airt_K <- airt +273.15 #convert to Kelvin
  # esat = 6.11*10^((7.5*airt)/(237.3+airt))
  ea <- 6.11*10^((7.5*dewt)/(237.3+dewt)) #https://www.weather.gov/media/epz/wxcalc/vaporPressure.pdf
  b = 0.433
  emiss_cs = 0.23 + b*(ea/airt_K)^(1/8)
  emiss_cl = 0.976
  tot_emiss = emiss_cs*(1-cc^2) + emiss_cl*(cc^2)
  sigma = 5.670373e-8 #Stefan Bolzmann constant
  lw_in = tot_emiss * sigma * (airt_K^4)
  return(lw_in)
}
