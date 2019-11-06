#' Set ACPy database
#'
#' Read ACPy xml configuration file and set name for the database to save the results.
#'
#' @param acpyXML XML file used for ACPy calibration
#' @param name character; name of file for database, must end with '.db'
#' @return Message saying what the database has been converted from
#' @import XML
#' @export
setAcpyDb <- function(acpyXML, name) {
  xml = xmlParse(acpyXML)
  node = getNodeSet(xml,'//transport')
  n = node[[1]]
  db1 = xmlAttrs(n)['path']
  xmlAttrs(n)['path'] = name
  saveXML(xml, acpyXML, indent = T)
  message('Changed database from ', db1,' to ',name)
}
