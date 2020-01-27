#' Plot monthly average profiles
#'
#' Plot monthly temperature averaged profiles for four months
#'
#' @param mod dataframe; Modelled values in the long format.
#' @param obs dataframe; Observed values in the long format.
#' @param year boolean; Facet by year. Defaults to FALSE
#' @param points boolean; Include points for observation depths. Defaults to FALSE
#' @return ggplot of temperature profiles
#' @importFrom lubridate year month
#' @export
plot_month_prof <- function(mod, obs, year = FALSE, points = FALSE){

  colnames(obs) <- c('date', 'depth', 'value')
  colnames(mod) <- c('date', 'depth', 'value')

  mod$fdepth <- factor(mod$depth)
  mod$month <- factor(month(mod$date))
  levels(mod$month) <- month.abb
  mod$year <- year(mod$date)
  ind = which(mod$year ==2010)

  obs$fdepth <- factor(obs$depth)
  obs$month <- factor(month(obs$date))
  levels(obs$month) <- month.abb
  obs$year <- year(obs$date)
  ind2 = which(obs$year ==2010)

  p1 <- ggplot(mod, aes(depth, value, colour = month))+
    stat_summary(aes(linetype = 'Mod'), fun.y = mean, geom = 'path')+
    stat_summary(data = obs, aes(linetype = 'Obs'), fun.y = mean, geom = 'path')+
    {if(points)stat_summary(data = obs, aes(shape = 'Obs'), fun.y = mean, geom = 'point')}+
    {if(year)facet_wrap(~year)}+
    scale_x_reverse()+
    coord_flip()+
    guides(colour = guide_legend(override.aes = list(size=4, alpha = 1, shape = 0)))+
    ylab('Depth (m)')+
    xlab('Temperature (degC)')+
    theme_bw(base_size = 16)
  return(p1)

}
