#' Set parameter ranges for ACPy
#'
#' Read ACPy xml configuration file and set ranges for the parameter
#'
#' @param acpyXML XML file used for ACPy calibration
#' @param param character corresponding to the name of the parameter in the ACPy xml file
#' @param min Minimum value for the parameter range
#' @param max Maximum value for the parameter range
#' @return Message stating the change in parameter range
#' @import XML
#' @export
setRange <- function(acpyXML,param,min,max) {
  xml = xmlParse(acpyXML)
  nodes = getNodeSet(xml,'//parameter')

  for(n in nodes) {
    if(xmlAttrs(n)['variable'] == param) {
      break
    }
  }
  mn = xmlAttrs(n)['minimum']
  mx = xmlAttrs(n)['maximum']
  xmlAttrs(n)['minimum'] = min
  xmlAttrs(n)['maximum'] = max
  saveXML(xml, acpyXML, indent = T)
  message('Changed range of ', param,' from ',mn,'-',mx, ' to ', min,'-',max)
}
