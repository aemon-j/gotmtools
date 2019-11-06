#' Extract dataframe of best parameters from DB
#'
#' Extract best set of calibrated parameters from database file with xml file
#'
#' @param dbFile database file; filepath to database file where calibration results are stored
#' @param acpyXML XML file; filepath to xml file used for ACPy calibration
#' @param run numeric; Run number to extract parameters from. If NULL extracts best parameters from the whole database. Defaults to NULL
#' @return data
#' @import RSQLite
#' @importFrom tidyr separate
#' @export
read_bestparam <- function(dbFile, acpyXML, run = NULL){
  xml = xmlParse(acpyXML)
  rootNode = xmlRoot(xml)
  data = xmlSApply(rootNode,function(x) xmlSApply(x, xmlAttrs))
  db = data$transports
  #dbFile = db["path",]
  #message('Database file = ', dbFile)
  params = data$parameters
  var.nam =c()
  for(p in params) {
    # if no values (ex: a comment):
    if(is.null(p)){
      next
    }
    if(!is.na(p["dummy"])){
      var.nam = append(var.nam, "dummy")
      next
    }
    var.nam = append(var.nam, p["variable"])
  }
  dbcon = dbConnect(dbDriver("SQLite"), dbname = dbFile) #Connect to the database
  table = dbGetQuery(dbcon,'select * from results' ) #Select the results
  dbDisconnect(dbcon) #Disconnect the database from R
  if(is.null(run)){
    bestId = which.max(table$lnlikelihood)
    x = strsplit(table$parameters[bestId],split = ';')[[1]]
    x = t(data.frame(calib = as.numeric(x)))
    colnames(x) = var.nam
    x = data.frame(x)
    message('Selected the best calibration from ', dbFile)
    return(x)
  }else{
    sub = table[(table$run == run),]
    bestId = which.max(sub$lnlikelihood)
    x = strsplit(sub$parameters[bestId],split = ';')[[1]]
    x = t(data.frame(calib = as.numeric(x)))
    colnames(x) = var.nam
    message('Selected the best calibration from ', dbFile, ' Run: ', run)
    x = data.frame(x)
    return(x)
  }
}
