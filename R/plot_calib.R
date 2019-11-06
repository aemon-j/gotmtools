#' Plot results from the database
#'
#' Creat a dotty plot of calibration results
#'
#' @param dbFile database file; filepath to the database file where calibration results are stored
#' @param acpyXML XML file; filepath to the xml file used for ACPy calibration
#' @param run numeric; Run number to extract parameters from. If NULL extracts best parameters from the whole database
#' @param plot.dim logical; Control the dimensions of the plot area using nrow and ncol
#' @param nrow numeric; Only used if plot.dim is set to TRUE. Designates how many rows are in the plot. Defaults to 2
#' @param ncol numeric; Only used if plot.dim is set to TRUE. Designates how many columns are in the plot. Defaults to 4
#' @param zoom logical; Applies a zoom factor to the plot. Defaults to FALSE
#' @param z.fct numeric; Multiplication factor on zoom to the top of the graph. Defaults to 2. Only used if zoom is set to TRUE.
#' @param update logical; Creates a plot that updates every ~30 seconds. It needs to be manually cancelled. Defaults to FALSE..
#' @return Plot of calibration
#' @import RSQLite
#' @import XML
#' @importFrom tidyr separate
#' @import graphics
#' @export
plot_calib <- function(dbFile, acpyXML,run = NULL, plot.dim = TRUE, nrow =2,ncol =4,
                       zoom = FALSE, z.fct =2, update =F){
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
  if(update == TRUE){
    for(i in 1:10000){
      Sys.sleep(30)
      try(dbcon <- dbConnect(dbDriver("SQLite"), dbname = dbFile)) #Connect to the database
      try(table <- dbGetQuery(dbcon,'select * from results')) #Select the results
      dbDisconnect(dbcon) #Disconnect the database from R
      p3 = separate(data = table, col = parameters, into = var.nam, sep = ";")
      n = which.max(p3[,ncol(p3)])
      runs = unique(p3$run)
      sim = nrow(p3)
      runseq = bquote(N == .(sim))
      if(plot.dim == FALSE){
        dims = ceiling(sqrt(length(var.nam)))
        nr = dims
        nc = dims
      }else{
        nr = nrow
        nc = ncol
      }
      par(mfrow = c(nr,nc))
      m = which(p3$run ==run)
      n = which(p3[,ncol(p3)] == max(p3[m,ncol(p3)], na.rm =T))
      if(zoom == TRUE){
        dif = (diff(range(p3$lnlikelihood[m], na.rm =T)))/z.fct
      }else{
        dif = (diff(range(p3$lnlikelihood[m], na.rm =T)))
      }
      for(j in 4:(ncol(p3)-1)){
        if(colnames(p3)[j] == 'k_min'){
          plot(p3[m,j], p3$lnlikelihood[m], main = paste(colnames(p3)[j],'; N =', nrow(p3[m,]), ';', signif(as.numeric(p3[n,j]),3),'; Run = ',run), ylab = 'LnLikelihood', col = 'blue',
               type ='p', pch =21, bg = 'blue', xlab ='', log ='x',cex = 0.75,
               ylim = c((max(p3$lnlikelihood[m], na.rm = TRUE) -dif),max(p3$lnlikelihood[m], na.rm =T)))
          abline(v = p3[n,j], lty=2, lwd =2, col =1)
        }else{
          plot(p3[m,j], p3$lnlikelihood[m], main = paste(colnames(p3)[j],'; N =', nrow(p3[m,]), ';', signif(as.numeric(p3[n,j]),3),'; Run = ',run), ylab = 'LnLikelihood', col = 'blue',
               type ='p', pch =21, bg = 'blue', xlab ='',cex = 0.75,
               ylim = c((max(p3$lnlikelihood[m], na.rm = TRUE) -dif),max(p3$lnlikelihood[m], na.rm = TRUE)))
          abline(v = p3[n,j], lty=2, lwd =2, col =1)
        }
      }
      message('Plot update attempted at: ', Sys.time())
    }
  }
  dbcon = dbConnect(dbDriver("SQLite"), dbname = dbFile) #Connect to the database
  table = dbGetQuery(dbcon,'select * from results' ) #Select the results
  dbDisconnect(dbcon) #Disconnect the database from R
  p3 = separate(data = table, col = parameters, into = var.nam, sep = ";")
  n = which.max(p3[,ncol(p3)])
  runs = unique(p3$run)
  sim = nrow(p3)
  runseq = bquote(N == .(sim))
  if(plot.dim == FALSE){
    dims = ceiling(sqrt(length(var.nam)))
    nr = dims
    nc = dims
  }else{
    nr = nrow
    nc = ncol
  }
  par(mfrow = c(nr,nc))
  if(is.null(run)){
    if(zoom == TRUE){
      dif = (diff(range(p3$lnlikelihood, na.rm =T)))/z.fct
    }else{
      dif = (diff(range(p3$lnlikelihood, na.rm =T)))
    }
    for(j in 4:(ncol(p3)-1)){
      if(colnames(p3)[j] == 'k_min'){
        plot(p3[,j], p3$lnlikelihood, main = paste(colnames(p3)[j],'; N =', sim, ';', signif(as.numeric(p3[n,j]),3)), ylab = 'LnLikelihood', col = 1,
             type ='p', pch =21, bg = 1, xlab ='', log ='x',cex = 0.75,
             ylim = c((max(p3$lnlikelihood, na.rm = TRUE) -dif),max(p3$lnlikelihood, na.rm =T)))
        for(k in 2:length(runs)){
          m = which(p3$run ==k)
          points(p3[m,j],p3$lnlikelihood[m], pch =21, col = k, bg =k, cex = 0.75)
        }
        abline(v = p3[n,j], lty=2, lwd =2, col ='saddlebrown')
      }else{
        plot(p3[,j], p3$lnlikelihood, main = paste(colnames(p3)[j],'; N =', sim, ';', signif(as.numeric(p3[n,j]),3)), ylab = 'LnLikelihood', col = 1,
             type ='p', pch =21, bg = 1, xlab ='',cex = 0.75,
             ylim = c((max(p3$lnlikelihood, na.rm = TRUE) -dif),max(p3$lnlikelihood, na.rm = TRUE)))
        for(k in 2:length(runs)){
          m = which(p3$run ==k)
          points(p3[m,j],p3$lnlikelihood[m], pch =21, col = k, bg =k,cex = 0.75)
        }
        abline(v = p3[n,j], lty=2, lwd =2, col ='saddlebrown')
      }
    }
  }else{
    m = which(p3$run ==run)
    n = which(p3[,ncol(p3)] == max(p3[m,ncol(p3)], na.rm =T))
    if(zoom == TRUE){
      dif = (diff(range(p3$lnlikelihood[m], na.rm =T)))/z.fct
    }else{
      dif = (diff(range(p3$lnlikelihood[m], na.rm =T)))
    }
    for(j in 4:(ncol(p3)-1)){
      if(colnames(p3)[j] == 'k_min'){
        plot(p3[m,j], p3$lnlikelihood[m], main = paste(colnames(p3)[j],'; N =', nrow(p3[m,]), ';', signif(as.numeric(p3[n,j]),3),'; Run = ',run), ylab = 'LnLikelihood', col = 'blue',
             type ='p', pch =21, bg = 'blue', xlab ='', log ='x',cex = 0.75,
             ylim = c((max(p3$lnlikelihood[m], na.rm = TRUE) -dif),max(p3$lnlikelihood[m], na.rm =T)))
        abline(v = p3[n,j], lty=2, lwd =2, col =1)
      }else{
        plot(p3[m,j], p3$lnlikelihood[m], main = paste(colnames(p3)[j],'; N =', nrow(p3[m,]), ';', signif(as.numeric(p3[n,j]),3),'; Run = ',run), ylab = 'LnLikelihood', col = 'blue',
             type ='p', pch =21, bg = 'blue', xlab ='',cex = 0.75,
             ylim = c((max(p3$lnlikelihood[m], na.rm = TRUE) -dif),max(p3$lnlikelihood[m], na.rm = TRUE)))
        abline(v = p3[n,j], lty=2, lwd =2, col =1)
      }
    }
  }
}
