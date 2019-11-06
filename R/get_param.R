#' Extract dataframe of all parameters from ACPy database
#'
#' Extract all parameters from database file with xml file
#'
#' @param dbFile database file; filepath to database file where calibration results are stored
#' @param acpyXML XML file; filepath to xml file used for ACPy calibration
#' @param run numeric; Run number to extract parameters from. If NULL extracts runs from the whole database. Defaults to NULL
#' @return data
#' @importFrom  RSQLite dbConnect dbGetQuery dbDisconnect
#' @importFrom  XML xmlParse xmlRoot xmlSApply
#' @importFrom tidyr separate
#' @importFrom readr parse_number
#' @export
get_param <- function(dbFile, acpyXML, run = NULL){
  xml = xmlParse(acpyXML)
  rootNode = xmlRoot(xml)
  data = xmlSApply(rootNode, function(x) xmlSApply(x, xmlAttrs))
  # db = data$transports
  params = data$parameters
  var.nam = c()
  for (p in params) {
    if (is.null(p)) {
      next
    }
    if (!is.na(p["dummy"])) {
      var.nam = append(var.nam, "dummy")
      next
    }
    var.nam = append(var.nam, p["variable"])
  }
  dbcon = dbConnect(dbDriver("SQLite"), dbname = dbFile)
  table = dbGetQuery(dbcon, "select * from results")
  dbDisconnect(dbcon)
  # pars <- data.frame(do.call('rbind', strsplit(as.character(table$parameters),';',fixed=TRUE)))
  pars <- separate(table, parameters, var.nam, sep = ';')
  pars[,c(var.nam)] <- sapply(pars[,c(var.nam)], as.numeric)
  if('extra_outputs' %in% colnames(pars)){
    spl <- strsplit(pars$extra_outputs[1], ",")[[1]]
    ext <- gsub("[^a-zA-Z]", "", spl)
    for(i in 1:length(ext)){
      pars[,(ncol(pars)+1)] <- NA
    }
    colnames(pars)[(ncol(pars) - (length(ext)-1)):ncol(pars)] <- ext
    for(i in 1:nrow(pars)){
      spl2 <- strsplit(pars$extra_outputs[i], ",")[[1]]
      out <- parse_number(spl2)
      for(j in 1:length(out)){
        pars[i, (ncol(pars) - length(out) +j)] <- out[j]
      }
    }
  }
  # pars <- mutate(pars, function(x) as.numeric(as.character(x)))
  # colnames(pars) <- var.nam
  pars$run <- factor(table$run)
  pars$id <- table$id
  pars$lnlikelihood <- table$lnlikelihood
  pars$time <- as.POSIXct(table$time, tz = 'UTC')
  if('extra_outputs' %in% colnames(pars)){
    pars <- pars[,c('id', 'run', 'time', var.nam, 'lnlikelihood',ext)]
    pars$extra_outputs <- NULL
  }else{
    pars <- pars[,c('id', 'run', 'time', var.nam, 'lnlikelihood')]
  }
  if(!is.null(run)){
    pars <- pars[(pars$run == run),]
  }
  return(pars)
}

