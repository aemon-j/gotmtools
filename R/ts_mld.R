#' Create a timeseries of mixed layer depth
#'
#' Calculates mixed layer depth (MLD) at each time step or on a daily time step. Returns NaN for steps where there is no MLD.
#'
#' @param wtr dataframe of water temperature in the long form as loaded through load.obs or formatted using match.mod()
#' @param slope a numeric vector corresponding to the minimum slope
#' @param seasonal logical; indicates whether the seasonal thermocline should be returned. This is fed to thermo.depth, which is used as the starting point. The seasonal thermocline is defined as the deepest density gradient found in the profile. If FALSE, the depth of the maximum density gradient is used as the starting point.
#' @param daily logical; if TRUE will calculate the MLD at a daily time step. Otherwise it will calculate it at the same timestep as the input data. Defaults to FALSE.
#' @param mixed.cutoff A cutoff (C) where below this threshold, thermo.depth and meta.depths are not calculated (NaN is returned). Defaults to 1 deg C.
#' @param tz Timezone string to be supplied to as.POSIXct. Defaults to 'UTC'. This often can be left to the default unless timezone support is specifically required.
#' @return data frame of calculated mixed layer depth.
#' @import rLakeAnalyzer
#' @export
ts_mld <- function(wtr, slope = 0.1, seasonal = TRUE, daily = F,
                   mixed.cutoff = 1, tz = 'UTC'){
  md.dat = c()
  tims = unique(wtr[,1])
  if(daily == T){
    dates = unique(format(tims, '%Y-%m-%d'))
    wtr$date = format(wtr[,1], '%Y-%m-%d')
    wtr[,2] = as.factor(wtr[,2])
    pb = txtProgressBar(min = 0, max = length(dates), style = 3)
    for(h in 1:length(dates)){
      ind = which(wtr$date == dates[h])
      #depths = as.vector(unique(-as.numeric(as.character(wtr[ind,2]))))
      wat = tapply(wtr[ind,3], wtr[ind,2],mean)
      depths = -as.numeric((dimnames(wat)[[1]][!is.na(wat)]))
      wat = as.vector(wat[!is.na(wat)])
      #wat = wat[length(wat):1]
      thermoD = thermo.depth(wat, depths, seasonal = seasonal,
                             mixed.cutoff = mixed.cutoff)
      if(is.nan(thermoD)){
        md.dat = append(md.dat,NaN)
        setTxtProgressBar(pb, h)
        next
      }
      rhoVar = water.density(wat)
      dRhoPerc = 0.15
      numDepths = length(depths)
      drho_dz = vector(mode = "double", length = numDepths - 1)
      for (i in 1:(numDepths - 1)) {
        drho_dz[i] = (rhoVar[i + 1] - rhoVar[i])/(depths[i +
                                                           1] - depths[i])
      }
      metaTop_depth = depths[1]
      Tdepth = rep(NaN, numDepths - 1)
      for (i in 1:(numDepths - 1)) {
        Tdepth[i] = mean(depths[i:(i + 1)])
      }
      tmp = sort.int(unique(c(Tdepth, thermoD)), index.return = TRUE)
      sortDepth = tmp$x
      sortInd = tmp$ix
      numDepths = length(sortDepth)
      drho_dz = stats::approx(Tdepth, drho_dz, sortDepth)
      drho_dz = drho_dz$y
      thermo_index = which(sortDepth == thermoD)
      for (i in seq(thermo_index, 1)) {
        if (!is.na(drho_dz[i]) && drho_dz[i] < slope) {
          metaTop_depth = sortDepth[i]
          break
        }
      }
      if (thermo_index - i >= 1 && (!is.na(drho_dz[thermo_index]) &&
                                    drho_dz[thermo_index] > slope)) {
        metaTop_depth = stats::approx(drho_dz[i:thermo_index],
                                      sortDepth[i:thermo_index], slope)
        metaTop_depth = metaTop_depth$y
      }
      if (is.na(metaTop_depth)) {
        metaTop_depth = depths[i]
      }
      md.dat = append(md.dat, metaTop_depth)
      setTxtProgressBar(pb, h)
    }
    close(pb)
    dates = as.POSIXct(dates, tz = tz)
    md.df = data.frame(date = dates, MLD = md.dat)
    return(md.df)
  }else if(daily == F){
    pb = txtProgressBar(min = 0, max = length(tims), style = 3)
    for(h in 1:length(tims)){
      ind = which(wtr[,1] == tims[h])
      depths = -wtr[ind,2]
      wat = wtr[ind,3]
      thermoD = thermo.depth(wat, depths, seasonal = seasonal,
                             mixed.cutoff = mixed.cutoff)
      if(is.nan(thermoD)){
        md.dat = append(md.dat,NaN)
        setTxtProgressBar(pb, h)
        next
      }
      rhoVar = water.density(wat)
      dRhoPerc = 0.15
      numDepths = length(depths)
      drho_dz = vector(mode = "double", length = numDepths - 1)
      for (i in 1:(numDepths - 1)) {
        drho_dz[i] = (rhoVar[i + 1] - rhoVar[i])/(depths[i +
                                                           1] - depths[i])
      }
      metaTop_depth = depths[1]
      Tdepth = rep(NaN, numDepths - 1)
      for (i in 1:(numDepths - 1)) {
        Tdepth[i] = mean(depths[i:(i + 1)])
      }
      tmp = sort.int(unique(c(Tdepth, thermoD)), index.return = TRUE)
      sortDepth = tmp$x
      sortInd = tmp$ix
      numDepths = length(sortDepth)
      drho_dz = stats::approx(Tdepth, drho_dz, sortDepth)
      drho_dz = drho_dz$y
      thermo_index = which(sortDepth == thermoD)
      for (i in seq(thermo_index, 1)) {
        if (!is.na(drho_dz[i]) && drho_dz[i] < slope) {
          metaTop_depth = sortDepth[i]
          break
        }
      }
      if (thermo_index - i >= 1 && (!is.na(drho_dz[thermo_index]) &&
                                    drho_dz[thermo_index] > slope)) {
        metaTop_depth = stats::approx(drho_dz[i:thermo_index],
                                      sortDepth[i:thermo_index], slope)
        metaTop_depth = metaTop_depth$y
      }
      if (is.na(metaTop_depth)) {
        metaTop_depth = depths[i]
      }
      md.dat = append(md.dat, metaTop_depth)
      setTxtProgressBar(pb, h)
    }
    close(pb)
    md.df = data.frame(date = tims, MLD = md.dat)
    return(md.df)
  }
}
