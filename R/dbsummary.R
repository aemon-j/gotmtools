#' Summarise parsac database
#'
#' Summarises all information within a database
#'
#' @param dbFile database file; filepath to database file with parsac results
#' @param parsacXML XML file; filepath to the xml file used for parsac calibration
#' @return dataframe containing summary information about the database
#' @import XML
#' @import RSQLite
#' @importFrom tidyr separate
#' @importFrom stats na.exclude
#' @export
dbsummary <- function(dbFile, parsacXML){
  xml = xmlParse(parsacXML)
  rootNode = xmlRoot(xml)
  data = xmlSApply(rootNode,function(x) xmlSApply(x, xmlAttrs))
  db = data$transports
  #dbFile = db["path",]
  #message('Database file = ', dbFile)
  params = data$parameters
  var.nam =c()
  for(p in params){
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
  runs = unique(table$run)
  df = data.frame(matrix(NA, nrow = length(runs), ncol = c(7+length(var.nam)),
                         dimnames = list(runs, c('Run','No._of_sims','No._of_parameters',
                                                 'Start_Time', 'End_Time', 'Max_lnlikelihood',
                                                 'No_of_NAs', var.nam))))
  for(i in runs){
    sub = table[(table$run == i),]
    df[i,1] = i
    df[i,2] = nrow(sub)
    df[i,3] = length(strsplit(sub$parameters[1],split = ';')[[1]])
    df[i,4] = sub$time[1]
    df[i,5] = sub$time[nrow(sub)]
    if(length(na.exclude(sub$lnlikelihood)) ==0){
      df[i,6] = NA
      df[i,8:ncol(df)] = NA
    }else{
      df[i,6] = max(sub$lnlikelihood,  na.rm = T)
      bestId = which.max(sub$lnlikelihood)
      x = strsplit(sub$parameters[bestId],split = ';')[[1]]
      df[i,8:ncol(df)] = as.numeric(x)
    }
    df[i,7] = sum(is.na(sub$lnlikelihood))
  }
  return(df)
}
