#' Calculate time-series over a depth dependent variable
#'
#' Calculate a time series of a statistical measure of modelled versus observed water temperature.
#'
#' @param mod dataframe; Modelled values in the long format. i.e. same as observed in ACPy
#' @param obs dataframe; Observed values in the long format loaded in using load.obs
#' @param stat 'character; which statistic to use. Options are 'RMSE', 'NSE','MAE' and 'pearson'
#' @param depth.range vector; vector with a depth range to extract RMSE at certain depths. Upper limit first e.g. c(-5,-10). Defaults to whole water column.
#' @param tz Timezone string to be supplied to as.POSIXct. Defaults to 'UTC'. This often can be left to the default unless timezone support is specifically required.
#' @return data frame; time series of RMSE
#' @importFrom hydroGOF NSE
#' @importFrom hydroGOF rmse
#' @export
ts_stat <- function(mod, obs,depth.range= NULL,stat = 'pearson', tz = 'UTC'){
  tims = unique(obs[,1])
  if(!is.null(depth.range)){
    obs = obs[(obs[,2] <= depth.range[1] & obs[,2] >= depth.range[2]),]
    mod = mod[(mod[,2] <= depth.range[1] & mod[,2] >= depth.range[2]),]
  }
  ob = aggregate(obs[,3], by = list(DateTime = cut(obs[,1], '1 hour')),mean)
  md = aggregate(mod[,3], by = list(DateTime = cut(mod[,1], '1 hour')),mean)
  df = data.frame(matrix(NA,ncol = 2, nrow = length(tims)))
  pb = txtProgressBar(min = 0, max = length(tims), style = 3)
  if(stat == 'RMSE'){
    for(i in 1:length(tims)){
      df[i,2] = sqrt(mean((md[i,2]- ob[i,2])^2))
      setTxtProgressBar(pb, i)
    }
  }
  if(stat == 'NSE'){
    for(i in 1:length(tims)){
      ind = which(obs[,1] == tims[i])
      df[i,2] = NSE(mod[ind,3], obs[ind,3])
      setTxtProgressBar(pb, i)
    }
  }
  if(stat == 'MAE'){
    for(i in 1:length(tims)){
      df[i,2] = mean(abs(md[i,2]-ob[i,2]))
      setTxtProgressBar(pb, i)
    }
  }
  if(stat == 'pearson'){
    for(i in 1:length(tims)){
      ind = which(obs[,1] == tims[i])
      pear_r = cor.test(obs[ind,3], mod[ind,3], method = 'pearson')
      df[i,2] = pear_r$estimate
      setTxtProgressBar(pb, i)
    }
  }
  close(pb)
  df[,1] = tims
  colnames(df) = c('date', stat)
  return(df)
}
