#' Plot monthly average profiles
#'
#' Plot monthly temperature averaged profiles for four months
#'
#' @param mod dataframe; Modelled values in the long format. i.e. same as observed in ACPy
#' @param obs dataframe; Observed values in the long format loaded in using load_obs
#' @param month vector; with string values for months for profiles in 3-letter format e.g. 'Jan'. Default = c('Jan','Apr', 'Jul','Oct')
#' @param year numeric; specifying year of profiles. Defaults to year of first measurement if not specified.
#' @param fixed.level logical; Temperature measurements are at fixed points throughout the timeseries. Defaults to TRUE
#' @param step numeric; Depth to average temperature measurements to. Only used if fixed.level = FALSE.
#' @return plot of temperature profiles
#' @importFrom lubridate year
#' @import utils
#' @importFrom stats aggregate
#' @import graphics
#' @export
plot_month_prof <- function(mod, obs, month = c('Jan','Apr', 'Jul','Oct'),
                            year = NULL, fixed.level =T, step = 2){
  if(is.null(year)){
    year = year(obs[1,1])
  }
  mod$yr = year(mod[,1])
  mod$month = format(mod[,1], '%b')
  obs$yr = year(obs[,1])
  obs$month = format(obs[,1], '%b')
  rng = range(obs[,3],mod[,3])
  ylm = c(min(obs[,2], mod[,2]),0)
  if(fixed.level == F){
    par(mfrow = c(2,2))
    for(i in 1:4){
      mod.1 = mod[(mod$yr ==year & mod$month == month[i]),]
      mod.1[,2] = round_any(mod.1[,2], step)
      mod.1 = aggregate(mod.1[,3] ~ mod.1[,2],FUN = mean)
      obs.1 = obs[(obs$yr ==year & obs$month == month[i]),]
      obs.1[,2] = round_any(obs.1[,2], step)
      obs.1 = aggregate(obs.1[,3] ~ obs.1[,2], FUN = mean)

      plot(obs.1[,2], obs.1[,1], type ='b', xlim = rng, main = paste0(month[i],' - ', year),
           ylab = 'Depth (m)', xlab = 'C', ylim = ylm)
      lines(mod.1[,2], mod.1[,1],type = 'b', col =2)
      legend('bottomright', legend = c('Obs', 'Mod'), col = c(1,2), lty = 1)
    }
  }
  if(fixed.level == T){
    par(mfrow = c(2,2))
    for(i in 1:4){
      mod.1 = mod[(mod$yr ==year & mod$month == month[i]),]
      mod.1 = aggregate(mod.1[,3] ~ mod.1[,2],FUN = mean)
      obs.1 = obs[(obs$yr ==year & obs$month == month[i]),]
      obs.1 =aggregate(obs.1[,3] ~ obs.1[,2], FUN = mean)

      plot(obs.1[,2], obs.1[,1], type ='b', xlim = rng, main = paste0(month[i],' - ', year),
           ylab = 'Depth (m)', xlab = 'C', ylim = ylm)
      lines(mod.1[,2], mod.1[,1],type = 'b', col =2)
      legend('bottomright', legend = c('Obs', 'Mod'), col = c(1,2), lty = 1)
    }
  }
}
