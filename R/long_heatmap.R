#' Create heatmap from observed files for acpy
#'
#' This function runs creates an interpolated heatmap from ACPy input files.
#' The level of interpolation can be adjusted to increase the accuracy
#' of the plot.
#'
#' @param obs dataframe; observations loaded with load.obs
#' @param title character; Title of the graph. Defaults to 'Heatmap'
#' @param ylim vector; Limits for the y-axis. Defaults to range of depths in the data
#' @param xlim vector; Limits for the x-axis. Defaults to range of values in the data
#' @return Plot of interpolated heatmap
#' @importFrom colorRamps matlab.like2
#' @import lubridate
#' @import ggplot2
#' @importFrom MBA mba.surf
#' @importFrom akima interp
#' @importFrom akima interp2xyz
#' @importFrom reshape2 melt
#' @importFrom dplyr filter

#' @export
#'
long_heatmap <- function(obs, title = 'Heatmap',ylim = NULL, xlim = NULL,size =1, zlab = '', ...){
  colnames(obs) = c('date', 'dep','temp')
  my.cols = RColorBrewer::brewer.pal(11, 'Spectral') #Colours for temp plot
  if(is.null(ylim)){
    ylim = range(obs[,2])
  }
  if(is.null(xlim)){
    xlim = range(obs[,1])
  }
  Fig <- ggplot(obs, aes(x = date, y = dep, color = temp)) +
    geom_point(size = size)+
    ggtitle(title)+
    ylab('Depth (m)')+
    scale_color_gradientn(colors = rev(my.cols), name = zlab, ...)+
    coord_cartesian(ylim = ylim, xlim = xlim)+
    labs(fill = zlab)+
    theme_bw()
  return(Fig)
}

# long.heatmap <- function(obs, title = 'Heatmap',facet =F,ncol =2,nrow =2,
#                          contour = F, points = F, var.depths =F, res =200,
#                          rev.y.ax =F,interp =T, ylim = NULL, zlab = '', ...){
#   colnames(obs) = c('date', 'dep','temp')
#   my.cols <- matlab.like2(7) #Colours for temp plot
#   if(is.null(ylim)){
#     ylim = range(obs[,2])
#   }
#   if(var.depths == T){
#     obs[,1] = decimal_date(obs[,1])
#     mba = mba.surf(obs, res, res)
#     dimnames(mba$xyz.est$z) = list(mba$xyz.est$x, mba$xyz.est$y)
#     df3 = melt(mba$xyz.est$z, varnames = c('date', 'depth'), value.name = 'temp')
#
#     if(facet ==T){
#       df3$year = year(date_decimal(df3[,1]))
#       df3$dec = df3[,1]-trunc(df3[,1])
#       obs$dec = obs[,1] - trunc(obs[,1])
#       obs$year = year(date_decimal(obs[,1]))
#       Fig =
#         ggplot(data=df3, aes(dec, depth))+
#         ggtitle(title)+
#         geom_raster(aes(fill = temp), interpolate = T) +
#         {if(contour == T)
#           geom_contour(aes(z = temp))
#         }+
#         {if(points == T)
#           geom_point(data = obs, aes(dec, dep), colour = 'white',
#                      size =0.001, inherit.aes = T)
#         }+
#         {if(rev.y.ax == T)
#           scale_y_reverse()
#         }+
#          scale_fill_gradientn(colours = matlab.like2(7), ...)+
#          facet_wrap(~year, ncol = ncol, nrow = nrow) +
#         coord_cartesian(ylim = ylim)+
#         labs(fill = zlab)+
#         theme_bw()
#       return(Fig)
#     }
#     Fig =
#       ggplot(data=df3, aes(date, depth))+
#       ggtitle(title)+
#       geom_raster(aes(fill = temp), interpolate = T, hjust = 0.5, vjust = 0.5) +
#       {if(contour == T)
#         geom_contour(aes(z = temp))
#       }+
#       {if(points == T)
#         geom_point(data = obs, aes(date, dep), colour = 'white', size =0.001)
#       }+
#       {if(rev.y.ax == T)
#         scale_y_reverse()
#       }+
#       scale_fill_gradientn(colours = matlab.like2(7), ...)+
#       coord_cartesian(ylim = ylim)+
#       labs(fill = zlab)+
#       theme_bw()
#     return(Fig)
#   }
#
#   if(interp ==F){
#     if(facet == T){
#       obs$year = year(obs[,1])
#       obs$yday = yday(obs[,1])
#       p = ggplot(obs, aes(x = yday, y = dep, z = temp, fill = temp)) +
#         geom_tile() +
#         ggtitle(title)+
#         {if(rev.y.ax == T)
#           scale_y_reverse()
#         }+
#         {if(contour == T)
#           geom_contour(aes(z = temp), inherit.aes = T)
#         }+
#         scale_fill_gradientn(colours = my.cols, na.value = 'gray', ...) +
#         facet_wrap(~year, ncol = ncol, nrow = nrow) +
#         ylab('Depth (m)')+
#         xlab('Time')+
#         coord_cartesian(ylim = ylim)+
#         labs(fill = zlab)+
#         theme_bw()
#       return(p)
#     }else{
#       p = ggplot(obs, aes(x = date, y = dep, z = temp, fill = temp)) +
#         geom_tile() +
#         ggtitle(title)+
#         {if(rev.y.ax == T)
#           scale_y_reverse()
#         }+
#         {if(contour == T)
#           geom_contour(aes(z = temp), inherit.aes = T)
#         }+
#         scale_fill_gradientn(colours = my.cols, na.value = 'gray', ...) +
#         ylab('Depth (m)')+
#         xlab('Time')+
#         coord_cartesian(ylim = ylim)+
#         labs(fill = zlab)+
#         theme_bw()
#       return(p)
#     }
#   }
#   tim = unique(obs[,1])
#   tdiff <- difftime(tim[2], tim[1], units = 'day')
#   if(tdiff<1){
#     obs$day = as.Date(obs[,1])
#     obs = aggregate(temp ~ day*dep, data = obs, FUN = mean)
#     obs[,1] = as.POSIXct(obs[,1], tz = 'UTC')
#   }
#   colnames(obs) = c('date', 'dep','temp')
#
#   if(points ==T & facet ==T){
#     obs$yday = yday(obs$date)
#   }
#
#   dat.interp = interp(x = obs$date, y = obs$dep, z = obs$temp,
#                        xo= seq(min(obs$date), max(obs$date), by = 86400), yo = seq(0, min(obs$dep), by = -1),
#                        extrap = FALSE, linear = TRUE)
#
#   dat.interp <- interp2xyz(dat.interp, data.frame = TRUE)
#   dat.interp[,1] = as.POSIXct(dat.interp[,1], origin = '1970-01-01', tz = 'UTC')
#   dat.interp = filter(dat.interp, x %in% obs$date)
#   dat.interp$yday <- yday(dat.interp[,1])
#   dat.interp$year = year(dat.interp[,1])
#
#   if(facet == T){
#     p = ggplot(dat.interp, aes(x = yday, y = y, z = z, fill = z)) +
#       geom_tile() +
#       ggtitle(title)+
#       {if(rev.y.ax == T)
#         scale_y_reverse()
#       }+
#       {if(contour == T)
#         geom_contour(aes(z = z), inherit.aes = T)
#       }+
#       {if(points == T)
#         geom_point(data = obs, aes(yday, dep), colour = 'white', size =0.001, inherit.aes = F)
#       }+
#       scale_fill_gradientn(colours = my.cols, na.value = 'gray', ...) +
#       facet_wrap(~year, ncol = ncol, nrow = nrow) +
#       ylab('Depth (m)')+
#       xlab('Time')+
#       coord_cartesian(ylim = ylim)+
#       labs(fill = zlab)+
#       theme_bw()
#     return(p)
#   }else{
#     p = ggplot(dat.interp, aes(x = x, y = y, z = z, fill = z)) +
#       geom_tile() +
#       ggtitle(title)+
#       {if(rev.y.ax == T)
#         scale_y_reverse()
#       }+
#       {if(contour == T)
#         geom_contour(aes(z = z), inherit.aes = T)
#       }+
#       {if(points == T)
#         geom_point(data = obs, aes(date, dep), colour = 'white', size =0.001, inherit.aes = F)
#       }+
#       scale_fill_gradientn(colours = my.cols, na.value = 'gray', ...) +
#       ylab('Depth (m)')+
#       xlab('Time')+
#       coord_cartesian(ylim = ylim)+
#       labs(fill = zlab)+
#       theme_bw()
#     return(p)
#   }
# }
