#' Edit XML file
#'
#' Input values and file names into the xml file for scenario setup
#'
#' @param param dataframe; parameters with colnames as parameter names which correspond to the parameters in the xml file
#' @param xmlFile XML file; for the parameters to be input into
#' @param print logical; prints changes to .xml fiel to the console. Defaults to TRUE.
#' @return data
#' @importFrom XML xmlParse
#' @importFrom XML saveXML
#' @export
input_xml <- function(param, xmlFile, print = T){
  var = colnames(param)
  xml = xmlParse(xmlFile)
  for(i in var){
    x = xpathSApply(xml, paste0("//",i),xmlValue)
    nodes = getNodeSet(xml, paste0("//",i)) #selects node
    if(length(nodes) == 0){
      message('Parameter "',i,'" is not present in ',xmlFile)
      next
    }
    lapply(nodes, function(n) {
      xmlValue(n) = gsub(x,param[1,i],xmlValue(n))
    })
    if(print == T){
      message('For parameter "',i,'" - Replaced ',x,' with ',param[1,i])
    }
  }
  saveXML(xml, xmlFile, indent = T)
  message(xmlFile, ' has been updated!')
}
