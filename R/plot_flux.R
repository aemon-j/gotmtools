#' Plot fluxes from NetCDF file
#'
#' Plots fluxes from the netCDF output file; Sensible heat flux (Qe), latent heat flux (Qh), long-wave back radation (Qb), incoming solar radiation (Qsw) and plots the net sum of all these fluxes (Net).
#'
#' @param ncdf filepath; Name of the netCDF file to extract variable
#' @param title character; Title of the graph. Defaults to 'Heat Fluxes'.
#' @param agg.dly logical; Aggregate the fluxes to a daily timestep.
#' @return 2 plots, one with the four fluxes and one with the net flux.
#' @importFrom ncdf4 nc_open
#' @importFrom ncdf4 nc_close
#' @importFrom ncdf4 ncvar_get
#' @importFrom ncdf4 ncatt_get
#' @importFrom reshape2 melt
#' @importFrom gridExtra grid.arrange
#' @import ggplot2
#' @export
plot_flux <- function(ncdf, title = 'Heat Fluxes', agg.dly = FALSE){
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
  qsw = ncvar_get(fid, 'I_0') #Albedo and swr_factor is included in this calculation
  net = qe + qh + qb + qsw
  nc_close(fid)

  #Extract time and formate Date
  df <- data.frame(DateTime = time, Qe = qe, Qh = qh, Qb = qb, Qsw = qsw, Net = net)
  if(agg.dly == T){
    df = aggregate(list(Qe = df$Qe, Qh = df$Qh, Qb = df$Qb, Qsw = df$Qsw, Net = df$Net), by = list(DateTime = cut(df$DateTime, '1 day')), mean)
    df$DateTime <- as.POSIXct(df$DateTime, tz = 'UTC')
  }
  dfmlt <- reshape2::melt(df, id.vars = 'DateTime')
  colnames(dfmlt) <- c('DateTime', 'Flux', 'value')
  #Plot data
  p1 <- ggplot(dfmlt[(dfmlt$Flux != 'Net'),], aes(DateTime, value, colour = Flux))+
    geom_hline(yintercept = 0, colour = 'black', linetype = 'dashed')+
    geom_line(size = 0.5)+
    ggtitle(title)+
    xlab('')+
    ylab('W/m^2')+
    theme_bw(base_size = 18)+
    guides(colour = guide_legend(override.aes = list(size=4, alpha = 1, shape = 0)))#+
  #theme(legend.justification = c(1, 1), legend.position = c(1, 1),legend.text=element_text(size=10), legend.title = element_text(size = 10), legend.c)

  p2 <- ggplot(dfmlt[(dfmlt$Flux == 'Net'),], aes(DateTime, value, colour = Flux))+
    geom_hline(yintercept = 0, colour = 'black', linetype = 'dashed')+
    geom_line()+
    ggtitle('Net Heat Flux')+
    xlab('')+
    ylab('W/m^2')+
    scale_colour_manual(values = 'black')+

    theme_bw(base_size = 18)

  return(grid.arrange(p1,p2, nrow =2))
}
