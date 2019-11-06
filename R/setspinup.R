#' Set spin up year for ACPy
#'
#' Read ACPy xml configuration file and set number of spin up years
#'
#' @param acpyXML XML file used for ACPy calibration
#' @param years numeric; no. of spin up years for ACPy calibration
#' @return Message stating what the file name change was
#' @import XML
#' @export
setspinup <- function(acpyXML,years) {
  xml = xmlParse(acpyXML)
  nodes = getNodeSet(xml,'//variable')
  yr = xmlAttrs(nodes[[1]])['spinupyears']
  xmlAttrs(nodes[[1]])['spinupyears'] = years
  saveXML(xml, acpyXML, indent = T)
  message('Changed spinup year from ', yr,' to ', years)
}
