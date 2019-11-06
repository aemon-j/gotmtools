#' Extract values at a depth or series of depths
#'
#' Extracts a variable at a certain depth or series of depths from netCDF or from text output
#'
#' @param mod.val dataframe of modelled values loaded using load.3d
#' @param mod.dep dataframe of depths of corresponding modelled values loaded
#' @param ncdf filepath; Name of the netCDF file to extract variable
#' @param var character; Name of the variable to be extracted. Must match short names in the netcdf file. Use list_vars() to view variables
#' @param depths vector of depth(s) for which to extract the value at
#' @param method character determining method for extracting depths at interpolated temperatures. Either linear interpolation or cubic spline. Defaults to linear.
#' @param print logical; Print a progress bar to the console. Defaults to TRUE
#' @return data frame in the long format the same as load.obs(). With modelled values at the selected depths.
#' @importFrom glmtools get_var
#' @import utils
#' @import stats
#' @export
extractDepths <- function(mod.val, mod.dep, ncdf = NULL, var = NULL, depths = NULL, method = 'linear', print = T){
  if(!is.null(ncdf)){
    mod.val = get_vari(ncdf, var, print = F)
    mod.dep = get_vari(ncdf, 'z', print = F)
  }
  dep = c()
  tmp = c()
  dat = c()
  pb = txtProgressBar(min = 0, max = nrow(mod.val), style = 3)
  for(i in 1:nrow(mod.val)){#nrow(mod)
    deps = depths
    y = mod.val[i,2:ncol(mod.val)]
    x = mod.dep[i,2:ncol(mod.dep)]
    if(method == 'linear'){
      tmp = append(tmp, approx(x,y,deps)$y)
    }else if(method == 'spline'){
      sm = smooth.spline(x = x, y = y, df = 4, spar = 0.3)
      tmp = append(tmp,predict(sm, deps)$y)
    }else{
      print('Specify method for interpolation')
    }
    dep = append(dep, deps)
    dat = append(dat,mod.val[i,1])
    if(print == T){
      setTxtProgressBar(pb, i)
    }
  }
  close(pb)
  df = data.frame(date = dat, depths = dep, value = tmp)
  return(df)
}
