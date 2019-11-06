#' Read ACPy configuration file and extract ranges
#'
#' Read ACPy xml configuration file and extracts parameters and ranges in a dataframe
#'
#' @param acpyXML XML file used for ACPy calibration
#' @return data frame with parameter names and ranges
#' @import XML
#' @import stats
#' @export
readAcpyParams <- function(acpyXML) {
  xml = xmlParse(acpyXML)
  paramsdf = data.frame(matrix(NA, nrow = 100, ncol = 3, dimnames = list(c(1:100),c('parameter','min','max'))))
  rootNode = xmlRoot(xml)
  data = xmlSApply(rootNode,function(x) xmlSApply(x, xmlAttrs))
  params = data$parameters
  for(p in params) {
    # if no values (ex: a comment):
    if(is.null(p)) next
    # if dummy:
    #if(is.na(p["file"])) next
    v = which(is.na(paramsdf[,1]))[1]
    paramsdf[v,1] = p["variable"]
    paramsdf[v,2] = p["minimum"]
    paramsdf[v,3] = p["maximum"]
  }
  paramsdf = na.exclude(paramsdf)
  return(paramsdf)
}
