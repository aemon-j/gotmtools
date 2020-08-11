#' Extract variables from NetCDF file for a certain depth
#'
#' Extracts a selected parameter from the netCDF file for a certain depth and formats it into a dataframe.
#'
#' @param ncdf filepath; Name of the netCDF file to extract variable
#' @param var character; Name of the variable to be extracted. Must match short name in netCDF file
#' @param z numeric; Depth at which the variable should be extracted. Must be negative.
#' @param constant_z; If set to TRUE, only the first row of the depth data is used. If FALSE, the depth data is checked for varying water levels. Defaults to FALSE.
#' @return dataframe/data.table
#' @import data.table
#' @author 
#' Jorrit Mesman
#' @examples
#' '\dontrun{
#' get_vari_z("output.nc", "temp", z = -1)
#' }
#' @export

get_vari_z = function(ncdf, var, z, constant_z = F){
  
  if(z>0) stop("z should be negative!")
  
  ### Load GOTM output
  dfVar = get_vari(ncdf, var, print = F)
  dfDepth = get_vari(ncdf, "z", print = F)
  
  # If water level doesn't change, only the first row of dfDepth is used, which speeds up computation
  if(constant_z | nrow(unique(dfDepth[, -1])) == 1){
    constant_z = T
    
    dfDepth = dfDepth[1,]
  }
  
  # Get the average column at which the depth is
  colNr = sapply(1:nrow(dfDepth), function(x) approx(dfDepth[x, 2:ncol(dfDepth)], 2:ncol(dfDepth), xout = z)$y)
  
  # Make a data table with the column numbers and their weights (weights based on linear interpolation)
  dfColNr = data.table(avCol = colNr,
                       lowCol = floor(colNr),
                       highCol = ceiling(colNr))
  dfColNr[, lowColWeight := 1 - (avCol - lowCol)]
  dfColNr[lowColWeight < 1, highColWeight := 1 - (highCol - avCol)]
  dfColNr[lowColWeight == 1, highColWeight := 0]
  
  # Create empty data table to return
  dfReturn = data.table(datetime = dfVar[[1]])
  
  if(constant_z){
    # Fill the (var) column with the interpolated values
    dfReturn[, (var) := dfVar[, dfColNr[["lowCol"]][1]] * dfColNr[["lowColWeight"]][1] +
               dfVar[, dfColNr[["highCol"]][1]] * dfColNr[["highColWeight"]][1]]
  }else{
    # Fill the (var) column with the interpolated values, with an apply-function
    dfReturn[, (var) := sapply(1:nrow(dfColNr),
                               function(x){dfVar[x, dfColNr[["lowCol"]][x]] * dfColNr[["lowColWeight"]][x] +
                                   dfVar[x, dfColNr[["highCol"]][x]] * dfColNr[["highColWeight"]][x]})]
  }
  
}

