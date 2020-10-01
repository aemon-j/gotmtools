#' Extract modelled temperature at observation depths
#'
#' Interpolates linearly ( or cubic spline) between model temperatures and extracts modelled temperatures at the same depths as observed temperatures or at specified depths.
#'
#' @param mod.val dataframe of modelled values loaded using load.3d
#' @param mod.dep dataframe of depths of corresponding modelled values loaded using load.depths and corrected using normDepths()
#' @param obs dataframe of observations loaded in using load_obs. Default = NULL
#' @param depths vector; Depths at which to extract observed temperature (negative). Used instead of extracting depths from obs dataframe.
#' @param method character determining method for extracting depths at interpolated temperatures. Either linear interpolation or cubic spline. Defaults to linear.
#' @param print logical; Print a progress bar to the console. Defaults to TRUE
#' @return data frame in the long format the same as load.obs(). With modelled values at the same depths as observed values.
#' @import stats
#' @import utils
#' @export
setmodDepths <- function(mod.val, mod.dep, obs = NULL, depths, method = "linear", print = T){
  if(is.null(obs)){
    deps = depths
    
    if(method == "linear"){
      tmp2 = lapply(1:nrow(mod.val), function(i) 
        approx(mod.dep[i, 2:ncol(mod.dep)], mod.val[i, 2:ncol(mod.val)], deps, rule = 2)$y)
    }else if(method == "spline"){
      tmp2 = lapply(1:nrow(mod.val), function(i) 
        predict(smooth.spline(x = mod.dep[i, 2:ncol(mod.dep)], y = mod.val[i, 2:ncol(mod.val)],
                              df = 4, spar = 0.3),
                deps)$y)
    }else{
      print("Specify method for interpolation")
    }
    
    dep = rep(deps, nrow(mod.val))
    dates = lapply(1:nrow(mod.val), function(i) rep(mod.val[i,1], length(deps)))
    
    df = data.frame(date = do.call("c", dates), depths = unlist(dep), value = unlist(tmp2))
    return(df)
    
  }else{
    dep = c()
    tmp = c()
    dates = c()
    pb = txtProgressBar(min = 0, max = nrow(mod.val), style = 3)
    
    for (i in 1:nrow(mod.val)) {
      ind = which(obs[, 1] == mod.val[i, 1])
      deps = obs[ind, 2]
      
      y = mod.val[i, 2:ncol(mod.val)]
      x = mod.dep[i, 2:ncol(mod.dep)]
      if (method == "linear"){
        tmp = append(tmp, approx(x, y, deps, rule = 2)$y)
      }else if (method == "spline"){
        sm = smooth.spline(x = x, y = y, df = 4, spar = 0.3)
        tmp = append(tmp, predict(sm, deps)$y)
      }else{
        print("Specify method for interpolation")
      }
      dep = append(dep, deps)
      dates = append(dates, rep(mod.val[i,1], length(deps)))
      if (print == T) {
        setTxtProgressBar(pb, i)
      }
    }
    close(pb)
    df = data.frame(date = dates, depths = dep, value = tmp)
    return(df)
    
  }
}
