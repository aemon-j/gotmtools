#' Plot Meteo data
#'
#' Plots meteorlogical variables used for input into GOTM
#'
#' @param meteo_file value file; File path to GOTM meteorlogical input file.
#' @param header logical; indicating whether the file contains the names of the variables as its first line. Defaults to FALSE.
#' @param sep character; field separator character. Values on each line of the file are separated by this character. Defaults to tab
#' @param start string; Start date of range of data to be plotted.
#' @param stop string; Stop date of range of data to be plotted.
#' @param tz Timezone string to be supplied to as.POSIXct. Defaults to 'UTC'. This often can be left to the default unless timezone support is specifically required.
#' @return Dataframe with Date time and values in wide format
#' @importFrom reshape2 melt
#' @importFrom utils read.delim
#' @export
plot_meteo <- function(meteo_file,header =F, sep = "\t", start = NULL, stop = NULL,tz ='UTC'){
  met = read.delim(meteo_file, header = header, sep =sep)
  if(!is.numeric(met[1,2])){
    met$DateTime = paste(met[,1], met[,2])
    met = met[,c(ncol(met),3:8)]
  }else{
    colnames(met)[1] <- 'DateTime'
  }
  met$DateTime <- as.POSIXct(met$DateTime, tz = tz)
  if(max(met[,6]) > 50){
    c6 = 'RelH'
    #ylab = '%'
  }else{
    c6 = 'DewT'
    #ylab = 'C'
  }

  colnames(met)[1] <- c('Date')#,'u10','v10','MSLP','AirT',c6,'CC')
  if(!is.null(start) & !is.null(stop)){
    met = met[(met$Date >= start & met$Date < stop),]
  }
  met1 = melt(met,id.vars = 'Date')

  p1 <- ggplot(met1, aes(Date,value, color = variable))+
    geom_line(size =0.1)+
    geom_point(size =0.5)+
    facet_wrap(~variable, ncol=1, scales = 'free')+
    theme_bw()
  return(p1)
}

#   par(mfrow = c(5,1))
#   mx = max(met[,2], met[,3])
#   mn = min(met[,2], met[,3])
#   plot(met$Date, met[,2], type = 'p', pch ='.', main = 'u10 & v10',
#        xlab = 'Date', ylab = 'm/s', ylim = c(mn, mx), col = 3)
#   points(met$Date, met[,3], col =2, pch ='.')
#   abline(h = 0)
#   legend('topright', col = c(3,2), pch =16, legend = c('u10', 'v10'))
#   plot(met$Date, met[,4], main = 'MSLP',pch = '.', type ='p',
#        xlab = 'Date', ylab = 'mbar')
#   abline(h = 1013, lty =2, col =4)
#   plot(met$Date, met[,5], main = 'AirT',pch = '.', type ='p',
#        xlab = 'Date', ylab = 'C')
#   abline(h = 0, lty =2, col =4)
#   if(max(met[,6]) > 50){
#     main = 'RelH'
#     ylab = '%'
#   }else{
#     main = 'DewT'
#     ylab = 'C'
#   }
#   plot(met$Date, met[,6], main = main, pch = '.', type ='p',
#        xlab = 'Date', ylab = ylab)
#   abline(h = 0, lty =2, col =4)
#   plot(met$Date, met[,7], main = 'CC', pch = '.', type ='p',
#        xlab = 'Date', ylab = 'fraction', ylim = c(0,1))
#	}
