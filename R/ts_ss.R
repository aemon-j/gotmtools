#' Calculate Schimidt Stability timeseries
#'
#' Calculate a time series of Schmidt stability for long form data with varying
#' lake level.
#'
#' @param wtr dataframe; loaded in using load.obs
#' @param bathy datframe; bathymetry loaded in using load.bathy from rLakeAnalyzer
#' @param fixed.level logical; Temperature measurements are at fixed points throughout the timeseries. Defaults to TRUE
#' @param tz Timezone string to be supplied to as.POSIXct. Defaults to 'UTC'. This often can be left to the default unless timezone support is specifically required.
#' @return data
#' @importFrom rLakeAnalyzer schmidt.stability
#' @export
ts_ss <- function(wtr, bathy,fixed.level = T, tz = 'UTC'){
  tims = unique(wtr[,1])
  ss.df = data.frame(matrix(NA,ncol = 2, nrow = length(tims)))
  pb = txtProgressBar(min = 0, max = length(tims), style = 3)
  #NEED TO look at if the first depth is = 0
  for(i in 1:length(tims)){
    if(fixed.level == T){
      ind = which(wtr[,1] == tims[i])
      ss.df[i,1] = format(tims[i],"%Y-%m-%d %H:%M:%S")
      ss.df[i,2] = schmidt.stability(wtr = as.vector(wtr[ind,3]),
                                     depths = as.vector(-wtr[ind,2]),
                                     bthA = as.vector(bathy[,2]),
                                     bthD = as.vector(bathy[,1]))
      setTxtProgressBar(pb, i)
    }else if(fixed.level == F){
      bathy2 = bathy
      ind = which(wtr[,1] == tims[i])
      #Apply correction to bath file first
      bdep = wtr[ind[length(ind)],2]
      diff = bathy[nrow(bathy),1] + bdep
      bathy2[,1] = bathy[,1]-diff
      ab = which(bathy2[,1] >0)
      ab = append(ab,(ab[1]-1))
      ab = ab[c(length(ab),1:(length(ab)-1))]
      bathy2 = bathy2[ab,]
      bathy2[1,1] = 0
      ss.df[i,1] = format(tims[i],"%Y-%m-%d %H:%M:%S")
      ss.df[i,2] = schmidt.stability(wtr = as.vector(wtr[ind,3]),
                                     depths = as.vector(-wtr[ind,2]),
                                     bthA = as.vector(bathy2[,2]),
                                     bthD = as.vector(bathy2[,1]))
      setTxtProgressBar(pb, i)
    }
  }
  close(pb)
  ss.df[,1] = as.POSIXct(ss.df[,1], tz = tz)
  colnames(ss.df) = c('date', 'Schmidt_stab_J_m2')
  return(ss.df)
}
