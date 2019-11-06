#' Plot water balance from NetCDF file
#'
#' Plots water balance calculated by GOTM ('Qres') and inflows from the netCDF output file.
#'
#' @param ncdf filepath; Name of the netCDF file to extract variable
#' @param title character; Title of the graph. Defaults to 'Water Balance
#' @return dataframe with the
#' @importFrom ncdf4 nc_open
#' @importFrom ncdf4 nc_close
#' @importFrom ncdf4 ncvar_get
#' @importFrom ncdf4 ncatt_get
#' @importFrom reshape2 melt
#' @import ggplot2
#' @export
plot_wbal <- function(ncdf, title = 'Water Balance'){

  vars_short = list_vars(ncdf,long = F)
  flows_nam = list(vars_short[grep('Q_', vars_short)])

  fid = nc_open(ncdf)
  tim = ncvar_get(fid, 'time')
  tunits = ncatt_get(fid,'time')
  #Extract time and formate Date
  lnam = tunits$long_name
  tustr <- strsplit(tunits$units, " ")
  step = tustr[[1]][1]
  tdstr <- strsplit(unlist(tustr)[3], "-")
  tmonth <- as.integer(unlist(tdstr)[2])
  tday <- as.integer(unlist(tdstr)[3])
  tyear <- as.integer(unlist(tdstr)[1])
  origin = as.POSIXct(paste0(tyear,'-',tmonth,'-',tday), format = '%Y-%m-%d', tz = 'UTC')
  time = as.POSIXct(tim, origin = origin, tz = 'UTC')

  flows = list()
  #Extract flows
  for(i in 1:length(flows_nam)){
    eval(parse(text = paste0("flows[[",i,"]] <- ncvar_get(fid, '",flows_nam[[i]],"')")))
  }
  flows_df <- data.frame(matrix(unlist(flows)),stringsAsFactors=FALSE)
  colnames(flows_df) <- flows_nam

  #Extract GOTM water balance
  qres = ncvar_get(fid, 'Qres')
  qres = qres[nrow(qres),]
  tunits = ncatt_get(fid, 'Qres')
  nc_close(fid)

  #Extract time and formate Date
  df <- data.frame(DateTime = time, GOTM_calc = qres)
  df <- cbind.data.frame(df, flows_df)
  dfmlt <- reshape2::melt(df, id.vars = 'DateTime')
  colnames(dfmlt) <- c('DateTime', 'Flow', 'value')
  #Plot data
  p1 <- ggplot(dfmlt, aes(DateTime, value, colour = Flow))+
    geom_line(size = 0.8)+
    ggtitle(title)+
    xlab('')+
    geom_hline(yintercept = 0, colour = 'black')+
    ylab(tunits$units)+
    theme_bw(base_size = 18)
  p1

  return(p1)
}
