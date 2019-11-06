#' Match the timestep of two dataframes
#'
#' Match the timestep of two different time series dataframes so they have the same number of steps. Works with long and wide-from data.
#' @param df1 dataframe; Data frame with timestep to be matched with.
#' @param df2 dataframe; Data frame with data to be matched to the timestep of df1
#' @return Dataframe of observation data subset to the same timestep as modelled data
#' @import utils
#' @export
match_tstep <- function(df1, df2){
  if(df1[1,1] == df1[2,1]){
    df = data.frame(DateTime = unique(df1[,1]))
    df = merge(df, df2, by = 1)
    return(df)
  }else{
	df = df2[(df2[,1] %in% df1[,1]),]
    return(df)
  }
}
