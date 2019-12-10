#' Create heatmap from dataerved files for acpy
#'
#' This function runs creates an interpolated heatmap from ACPy input files.
#' The level of interpolation can be adjusted to increase the accuracy
#' of the plot.
#'
#' @param data dataframe; dataervations loaded with load.data
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
#' @examples
#' sim_folder <- system.file('extdata', package = 'GOTMr')
#' run_gotm(sim_folder)
#' out <- file.path(sim_folder, 'output', 'output.nc')
#' wtemp <- get_vari(ncdf = out, var = 'temp')
#' z <- get_vari(ncdf = out, var = 'z')
#' df <- wide2long(data = wtemp, depths = z)
#' long_heatmap(data = df, title = 'Feeagh simulated temp', zlab = 'degC')
#' @export
#'
long_heatmap <- function(data, title = 'Heatmap',ylim = NULL, xlim = NULL,size =1, zlab = '', ...){
  colnames(data) = c('date', 'dep','temp')
  my.cols = RColorBrewer::brewer.pal(11, 'Spectral') #Colours for temp plot
  if(is.null(ylim)){
    ylim = range(data[,2])
  }
  if(is.null(xlim)){
    xlim = range(data[,1])
  }
  Fig <- ggplot(data, aes(x = date, y = dep, color = temp)) +
    geom_point(size = size)+
    ggtitle(title)+
    ylab('Depth (m)')+
    scale_color_gradientn(colors = rev(my.cols), name = zlab, ...)+
    coord_cartesian(ylim = ylim, xlim = xlim)+
    labs(fill = zlab)+
    theme_bw()
  return(Fig)
}

# long.heatmap <- function(data, title = 'Heatmap',facet =F,ncol =2,nrow =2,
#                          contour = F, points = F, var.depths =F, res =200,
#                          rev.y.ax =F,interp =T, ylim = NULL, zlab = '', ...){
#   colnames(data) = c('date', 'dep','temp')
#   my.cols <- matlab.like2(7) #Colours for temp plot
#   if(is.null(ylim)){
#     ylim = range(data[,2])
#   }
#   if(var.depths == T){
#     data[,1] = decimal_date(data[,1])
#     mba = mba.surf(data, res, res)
#     dimnames(mba$xyz.est$z) = list(mba$xyz.est$x, mba$xyz.est$y)
#     df3 = melt(mba$xyz.est$z, varnames = c('date', 'depth'), value.name = 'temp')
#
#     if(facet ==T){
#       df3$year = year(date_decimal(df3[,1]))
#       df3$dec = df3[,1]-trunc(df3[,1])
#       data$dec = data[,1] - trunc(data[,1])
#       data$year = year(date_decimal(data[,1]))
#       Fig =
#         ggplot(data=df3, aes(dec, depth))+
#         ggtitle(title)+
#         geom_raster(aes(fill = temp), interpolate = T) +
#         {if(contour == T)
#           geom_contour(aes(z = temp))
#         }+
#         {if(points == T)
#           geom_point(data = data, aes(dec, dep), colour = 'white',
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
#         geom_point(data = data, aes(date, dep), colour = 'white', size =0.001)
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
#       data$year = year(data[,1])
#       data$yday = yday(data[,1])
#       p = ggplot(data, aes(x = yday, y = dep, z = temp, fill = temp)) +
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
#       p = ggplot(data, aes(x = date, y = dep, z = temp, fill = temp)) +
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
#   tim = unique(data[,1])
#   tdiff <- difftime(tim[2], tim[1], units = 'day')
#   if(tdiff<1){
#     data$day = as.Date(data[,1])
#     data = aggregate(temp ~ day*dep, data = data, FUN = mean)
#     data[,1] = as.POSIXct(data[,1], tz = 'UTC')
#   }
#   colnames(data) = c('date', 'dep','temp')
#
#   if(points ==T & facet ==T){
#     data$yday = yday(data$date)
#   }
#
#   dat.interp = interp(x = data$date, y = data$dep, z = data$temp,
#                        xo= seq(min(data$date), max(data$date), by = 86400), yo = seq(0, min(data$dep), by = -1),
#                        extrap = FALSE, linear = TRUE)
#
#   dat.interp <- interp2xyz(dat.interp, data.frame = TRUE)
#   dat.interp[,1] = as.POSIXct(dat.interp[,1], origin = '1970-01-01', tz = 'UTC')
#   dat.interp = filter(dat.interp, x %in% data$date)
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
#         geom_point(data = data, aes(yday, dep), colour = 'white', size =0.001, inherit.aes = F)
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
#         geom_point(data = data, aes(date, dep), colour = 'white', size =0.001, inherit.aes = F)
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
