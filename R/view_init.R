#' View inital profile
#'
#' Prints initial temperature profile for the model
#'
#' @param file value file; File path to GOTM initial temperature file.
#' @param start string; Start date of model initialization.
#' @param header logical; indicating whether the file contains the names of the variables as its first line. Defaults to FALSE.
#' @param sep character; field separator character. Values on each line of the file are separated by this character. Defaults to tab
view_init <- function(file, start, header = FALSE, sep = "\t"){
  sta = as.POSIXct(start, tz ='UTC')
  tp = read.delim(file, header = header, col.names = c('Depth', 'Temp'), sep =sep)
  if(sep == ' '){
    tim = as.POSIXct(as.POSIXct(paste(tp[,1], tp[,2]), format = '%Y-%m-%d %H:%M:%S', tz = 'UTC'))
  }else if(sep == "\t"){
    tim = as.POSIXct(tp[,1], format = '%Y-%m-%d %H:%M:%S', tz = 'UTC')
  }
  ind = which(tim == sta)
  if(length(ind) == 0){
    message('No measured value at ', start,'. Nearest value is:')
    ind = which.min(abs(tim - sta))
  }
  if(sep == ' '){
    nrow = as.numeric(as.character(tp[(ind+1),1]))
    print(tp[ind:(ind+nrow+1),])
  }else if(sep == "\t"){
    nrow = as.numeric(strsplit(as.character(tp[ind,2]), ' ')[[1]][1])
    print(tp[ind:(ind+nrow),])
  }
  nrow = as.numeric(strsplit(as.character(tp[ind,2]), ' ')[[1]][1])
  print(tp[ind:(ind+nrow),])
}
