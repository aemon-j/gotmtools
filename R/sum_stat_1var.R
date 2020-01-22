#' Caclulate summary stats of one variable
#'
#' Calculate general summary statistics of one variable, modelled vs observed; Pearson's R, variance, covariance, bias, Nash-Sutcliffe Efficiency (NSE) and Root mean squared error (RMSE).
#'
#' @param mod vector; Modelled values
#' @param obs vector; Observed values
#' @param na.rm logical; Remove NA'values
#' @return data frame of summary statistics
#' @export
sum_stat_1var <- function(mod, obs, na.rm =T){
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
                             Variance_mod = var_mod,SD_obs = SD_obs, SD_mod = SD_mod,  Covariance = cov,
                             #Correlation =cor,
                             Bias = bias, MAE = mae, RMSE = rmse, NSE = nse, row.names = c())
  return(summary_stats)
}
