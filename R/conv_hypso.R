#' Convert hypsograph to rLakeAnalyzer format
#'
#' Convert GOTM hypsograph format to the format for use with rLakeAnalyzer
#'
#' @param in_file value file; File path to GOTM hypsograph file
#' @param out_file value file; File path to write the new ".bth' file to
#' @param header logical; indicating whether the file contains the names of the variables as its first line. Defaults to FALSE.
#' @param sep character; field separator character. Values on each line of the file are separated by this character. Defaults to tab
#' @return File with hypsograph formatted for rLakeAnalyzer
#' @importFrom utils read.delim
#' @importFrom utils write.csv
#' @export
conv_hypso <- function(in_file, out_file,header =F, sep = "\t"){
  hyp <- read.delim(in_file, header = F,sep = sep, stringsAsFactors = F)
  hyp <- hyp[-1,]
  hyp[,1] <- -hyp[,1]
  colnames(hyp) <- c('Bathymetry Depths','Bathymetry Areas')
  write.csv(hyp,out_file, row.names = F, quote = F)
}
