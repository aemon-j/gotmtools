#' Locate stratification or ice on/off dates
#'
#' Calculate dates of stratification or ice on/off dates. Ice on/off calculation is based on Pierson et al. (2011)
#' formula when the upper sensor records a temperature of at least -0.1C below that of the bottom sensor.
#' Stratification is based on the opposite principle i.e when there is a difference of +0.1C between the top and bottom sensor.
#'
#' @param data dataframe; Water temperature profile data in the long form as loaded in using load.obs
#' @param type character; Which dates to caclulate, either ice or stratification on/off. Defaults to 'strat'.
#' @param days numeric; no. of days of consecutive days before identifying a break. Defaults to 7.
#' @param Tmin numeric; Minimum temperature as a cutoff point for identifying breaks. Defaults to 0.1
#' @param tz Timezone string to be supplied to as.POSIXct. Defaults to 'UTC'. This often can be left to the default unless timezone support is specifically required.
#' @return vector with dates of stratification or ice on/off
#' @import utils
#' @export
key_dates <- function(data, type = 'strat', days =7, Tmin = 0.1, tz ='UTC'){
  tim1 = unique(data[,1])
  #years = unique(year(data[,1]))
  date = c()
  pb = txtProgressBar(min = 0, max = length(tim1), style = 3)
  if(type == 'ice'){
    names = c('ice off', 'ice on')
    for(i in 1:length(tim1)){
      ind = which(data[,1] == tim1[i]) ##
      deps = data[ind,2]
      tmp = data[ind, 3]
      mx = tmp[which.max(deps)]
      mn = tmp[which.min(deps)]
      if((mx-mn) <= -Tmin){
        if(is.null(date[length(date)])){
          date = append(date, tim1[i])
          next
        }
        date = append(date, tim1[i])
      }
      setTxtProgressBar(pb, i)
    }
    close(pb)
  }else if(type == 'strat'){
    for(i in 1:length(tim1)){
      ind = which(data[,1] == tim1[i]) ##
      deps = data[ind,2]
      tmp = data[ind, 3]
      mx = tmp[which.max(deps)]
      mn = tmp[which.min(deps)]
      if((mx-mn) >= Tmin){
        if(is.null(date[length(date)])){
          date = append(date, tim1[i])
          next
        }
        date = append(date, tim1[i])
      }
      setTxtProgressBar(pb, i)
    }
    close(pb)
  }
  attributes(date)$tzone = tz
  dat = difftime(date[2:length(date)],date[1:(length(date)-1)], units = 'hour')
  a1 = which(dat > 24*days)
  a2 = a1 +1
  a = date[c(1,a1,a2,length(date))]
  a = a[order(a)]
  return(a)
  # ice.on = c()
  # ice.off = c()
  # for(i in 1:length(a)){
  #   yr = year(a[i])
  #   mth = month(a[i])
  #   n = which(years ==yr)
  #   if(mth <=6){
  #     ice.off <- append(ice.off,format(a[i]))
  #   }else if(mth >6){
  #     ice.on <- append(ice.on,format(a[i]))
  #   }
  # }
  # dates = list(ice.off = ice.off, ice.on =ice.on)
  # return(dates)
}
