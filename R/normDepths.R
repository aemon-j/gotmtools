#' Normalize depths of modelled output
#'
#' For modelled data where there is a varying lake level, this normalises the depths so the surface is the same and assigns depths to the depths lower relative to the top depth. Allows for comparison with observed data from load.obs
#'
#' @param mod.dep dataframe of modelled values loaded using load.3d
#' @param print logical; Print a progress bar to the console. Defaults to TRUE
#' @return data frame of normalized modelled depths
#' @import utils
#' @export
normDepths <- function(mod.dep, print = T){
  norm.dep = matrix(NA, nrow = nrow(mod.dep), ncol = ncol(mod.dep))
  pb = txtProgressBar(min = 0, max = nrow(mod.dep), style = 3)
  for(i in 1:nrow(mod.dep)){
    for(j in 2:ncol(mod.dep)){
      norm.dep[i,j] = round(mod.dep[i,j] - mod.dep[i,2],1)
    }
    if(print == T){
      setTxtProgressBar(pb, i)
    }
  }
  norm.dep = data.frame(norm.dep)
  norm.dep[,1] = mod.dep[,1]
  return(norm.dep)
}
