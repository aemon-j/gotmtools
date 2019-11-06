#' Load GOTMobservation file
#'
#' Load observations which are used for calibration, for plotting in a heatmap
#' @param obs.file observation file; File path to GOTM observation file containing the time, depths and values of observations
#' @param header logical; file contains the names of the variables as its first line. Defaults to FALSE
#' @param sep character; the delimiter in the file. Defaults to tab.
#' @param tz Assigns timezone to the DateTime column. Defaults to 'UTC'
#' @return Dataframe of observation data for GOTM
#' @export
load_obs <- function(obs.file, header = F,sep = "\t", tz = 'UTC'){
  if(sep == ' '){
    obs = read.delim(obs.file, header = header, sep = sep)
    obs$date = paste(obs[,1], obs[,2])
    obs = obs[,c(5,3,4)]
    colnames(obs) = c('date','depths','value')
  }else{
    obs = read.delim(obs.file, header = header,sep = sep, col.names = c('date','depths','value'))
  }
  obs[,1] = as.POSIXct(as.character(obs[,1]),format = '%Y-%m-%d %H:%M:%S', tz = tz)
  return(obs)
}
