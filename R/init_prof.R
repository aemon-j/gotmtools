#' Create initial profile for GOTM
#'
#' Extract and format the initial profile for GOTM from the observation file used in ACPy.
#'
#' @param obs_file filepath; Path to observation file
#' @param date character; Date in "YYYY-mm-dd HH:MM:SS" format to extract the initial profile.
#' @param tprof_file filepath; For the new initial temperature profile file.
#' @param print logical; Prints the temperature profile to the con
#' @param ... arguments to be passed to read.delim() for reading in observed file e.g "header = TRUE, sep = ','"
#' @return Message stating if the file has been created
#' @import utils
#' @export
init_prof <- function(obs_file, date, tprof_file, print = TRUE, ...){
  obs <- read.delim(obs_file, stringsAsFactors = F, ...)
  #obs[,1] <- as.POSIXct(obs[,1], tz = 'UTC')
  dat = which(obs[,1] == date)
  ndeps = length(dat)
  deps = obs[dat,2]
  tmp = obs[dat,3]
  df <- matrix(NA, nrow =1+ndeps, ncol =2)
  df[1,1] <- date
  df[1,2] <- paste0(ndeps,' ',2)
  df[(2):(1+ndeps),1] = as.numeric(obs[dat,2])
  df[(2):(1+ndeps),2] = as.numeric(obs[dat,3])
  write.table(df, tprof_file, quote = F, row.names = F, col.names = F,
              sep = "\t")
  message('New inital temperature file ', tprof_file, ' has been created.')
  if(print == TRUE){
    print(df)
  }
}
