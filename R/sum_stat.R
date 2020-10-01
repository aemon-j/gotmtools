#' Caclulate summary stats of mod v obs
#'
#' Calculate general summary statistics of modelled water temperature vs observed water temperature; Pearson's R, variance, covariance, bias, Nash-Sutcliffe Efficiency (NSE) and Root mean squared error (RMSE) and log-likelihood (lnlikelihood).
#'
#' @param mod vector or dataframe; Vector if no depth values otherwise Modelled values in the long format; date, depth, values
#' @param obs vector or dataframe; Vector if no depth values otherwise Observed values in the long format; date, depth, values
#' @param depth logical; Depth values are included. Defaults to False
#' @param na.rm logical; Remove NA'values
#' @param depth.range vector; vector with a depth range to extract statistics at certain depths.
#' @return data frame of summary statistics
#' @export
sum_stat <- function(mod, obs, depth = FALSE, na.rm = TRUE, depth.range = NULL){
  if(depth == T){
    # Make depths positive
    obs[,2] <- abs(obs[,2])
    mod[,2] <- abs(mod[,2])

    if(!is.null(depth.range)){
      obs = obs[(obs[,2] <= max(depth.range) & obs[,2] >= min(depth.range)),]
      mod = mod[(mod[,2] <= max(depth.range) & mod[,2] >= min(depth.range)),]
    }

    df <- merge(mod, obs, by = c(1,2))
    df <- na.exclude(df)

    # analyse strat
    # o.strat <- analyse_strat(df[,c(1,2,4)])
    # m.strat <- analyse_strat(df[,c(1:3)])
    #
    # diff.strat <- o.strat
    # for(i in 1:nrow(o.strat)){
    #   for(j in 2:ncol(o.strat)){
    #     diff.strat[i,j] <- m.strat[i,j] - o.strat[i,j]
    #   }
    # }
    # diff <- t(as.data.frame(colMeans(diff.strat[,-1])))
    # rownames(diff) <- NULL


    colnames(df)[3:4] <- c('mod', 'obs')
    dif = df$mod - df$obs
    pear_r = cor.test(df$obs, df$mod, method = 'pearson')
    var_obs = mean(((df$obs-mean(df$obs, na.rm = na.rm))^2), na.rm = na.rm)
    var_mod = mean(((df$mod-mean(df$mod, na.rm = na.rm))^2), na.rm = na.rm)
    SD_obs = sd(df$obs, na.rm = na.rm)
    SD_mod = sd(df$mod, na.rm = na.rm)
    cov = mean((df$obs-mean(df$obs, na.rm = na.rm))*(df$mod-mean(df$mod, na.rm = na.rm)), na.rm = na.rm)
    cor = cov/sqrt(var_obs*var_mod)
    bias = mean(dif, na.rm = na.rm)
    mae = mean(abs(dif), na.rm = na.rm)
    rmse = sqrt(mean(dif^2, na.rm = na.rm))
    nse = NSE(df$mod, df$obs, na.rm = na.rm)
    lnlikelihood = sum(dnorm(df$obs, mean = df$mod, log = TRUE), na.rm = na.rm)
    summary_stats = data.frame(Pearson_r = pear_r$estimate,Variance_obs = var_obs,
                               Variance_mod = var_mod, SD_obs = SD_obs, SD_mod = SD_mod,
                               Covariance = cov, #Correlation =cor,
                               Bias = bias, MAE = mae, RMSE = rmse, NSE = nse, lnlikelihood = lnlikelihood, row.names = c())
    # summary_stats <- cbind.data.frame(summary_stats, diff)
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
    nse = NSE(mod, obs, na.rm = na.rm)
    lnlikelihood = sum(dnorm(obs, mean = mod, log = TRUE), na.rm = na.rm)
    summary_stats = data.frame(Pearson_r = pear_r$estimate,Variance_obs = var_obs,
                               Variance_mod = var_mod, SD_obs = SD_obs, SD_mod = SD_mod,
                               Covariance = cov, #Correlation =cor,
                               Bias = bias, MAE = mae, RMSE = rmse, NSE = nse, lnlikelihood = lnlikelihood, row.names = c())
    return(summary_stats)
  }

}
