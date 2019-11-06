#' Caclulate summary stats of mod v obs
#'
#' Calculate general summary statistics of modelled water temperature vs observed water temperature; Pearson's R, variance, covariance, bias, Nash-Sutcliffe Efficiency (NSE) and Root mean squared error (RMSE).
#'
#' @param mod vector or dataframe; Vector if no depth values otherwise Modelled values in the long format. i.e. same as observed in ACPy
#' @param obs vector or dataframe; Vector if no depth values otherwise Observed values in the long format loaded in using load.obs
#' @param depth logical; Depth values are included. Defaults to False
#' @param na.rm logical; Remove NA'values
#' @param depth.range vector; vector with a depth range to extract statistics at certain depths. Upper limit first e.g. c(-5,-10). Defaults to whole depth range.
#' @return data frame of summary statistics
#' @import stats
#' @export
sum_stat <- function(mod, obs, depth =F,na.rm =T, depth.range =NULL){
  if(depth == T){
    if(!is.null(depth.range)){
      obs = obs[(obs[,2] <= depth.range[1] & obs[,2] >= depth.range[2]),]
      mod = mod[(mod[,2] <= depth.range[1] & mod[,2] >= depth.range[2]),]
    }
    dif = mod[,3]- obs[,3]
    pear_r = cor.test(obs[,3], mod[,3], method = 'pearson')
    var_obs = mean(((obs[,3]-mean(obs[,3], na.rm = na.rm))^2), na.rm = na.rm)
    var_mod = mean(((mod[,3]-mean(mod[,3], na.rm = na.rm))^2), na.rm = na.rm)
    SD_obs = sd(obs[,3], na.rm = na.rm)
    SD_mod = sd(mod[,3], na.rm = na.rm)
    cov = mean((obs[,3]-mean(obs[,3], na.rm = na.rm))*(mod[,3]-mean(mod[,3], na.rm = na.rm)), na.rm = na.rm)
    cor = cov/sqrt(var_obs*var_mod)
    bias = mean(dif, na.rm = na.rm)
    mae = mean(abs(dif), na.rm = na.rm)
    rmse = sqrt(mean(dif^2, na.rm = na.rm))
    nse = NSE(mod[,3], obs[,3])
    summary_stats = data.frame(Pearson_r = pear_r$estimate,Variance_obs = var_obs,
                               Variance_mod = var_mod, SD_obs = SD_obs, SD_mod = SD_mod,
                               Covariance = cov, #Correlation =cor,
                               Bias = bias, MAE = mae, RMSE = rmse, NSE = nse, row.names = c())
    return(summary_stats)
  }else{
    dif = mod- obs
    pear_r = cor.test(obs, mod, method = 'pearson')
    var_obs = mean(((obs-mean(obs, na.rm = na.rm))^2), na.rm = na.rm)
    var_mod = mean(((mod-mean(mod, na.rm = na.rm))^2), na.rm = na.rm)
    SD_obs = sd(obs, na.rm = na.rm)
    SD_mod = sd(mod, na.rm = na.rm)
    cov = mean((obs-mean(obs, na.rm = na.rm))*(mod-mean(mod, na.rm = na.rm)), na.rm = na.rm)
    cor = cov/sqrt(var_obs*var_mod)
    bias = mean(dif, na.rm = na.rm)
    mae = mean(abs(dif), na.rm = na.rm)
    rmse = sqrt(mean(dif^2, na.rm = na.rm))
    nse = NSE(mod, obs)
    summary_stats = data.frame(Pearson_r = pear_r$estimate,Variance_obs = var_obs,
                               Variance_mod = var_mod, SD_obs = SD_obs, SD_mod = SD_mod,
                               Covariance = cov, #Correlation =cor,
                               Bias = bias, MAE = mae, RMSE = rmse, NSE = nse, row.names = c())
    return(summary_stats)
  }

}
