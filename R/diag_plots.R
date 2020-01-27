#' Produces model diagnostic plots
#'
#' Produces 6 plots to assess model performance.
#'
#' @param mod dataframe; modelled values in long format.
#' @param obs dataframe; observed values in long format.
#' @param size numeric; size of points in plot. Defaults to 0.1
#' @param ggplot logical; plot in ggplot or base plot. Defaults to TRUE
#' @param colourblind logical; Use colourblind friendly colours. Defaults to FALSE
#' @param na,rm boolean; a logical value indicating whether NA values should be stripped before the computation proceeds. Defaults to FALSE
#' @return grob object which can be assigned and then saved using ggsave() function
#' @importFrom RColorBrewer brewer.pal
#' @importFrom grDevices colorRamp
#' @import ggplot2
#' @import graphics
#' @export
diag_plots <- function(mod, obs, size = 0.1, ggplot = TRUE, colourblind = FALSE, na.rm = FALSE){
  stats = sum_stat(mod, obs, depth = T, na.rm = na.rm)
  df <- merge(mod, obs, by = c(1,2))
  colnames(df)[3:4] <- c('mod', 'obs')
  if(max(df[,2]) > 0){ #Makes depths negative
    df[,2] <- -df[,2]
  }
  ndep = length(unique(df[,2]))
  if(colourblind){
    dramp <- colorRampPalette(brewer.pal(8, 'Dark2'))
  }
  if(ggplot == FALSE){
    dif = df$mod - df$obs
    par(mfrow=c(2,3))

    xfit <- seq(min(dif, na.rm = T), max(dif, na.rm = T), length=40)
    yfit_density <- dnorm(xfit, mean=mean(0, na.rm = T), sd=sd(dif, na.rm = T))

    # frequency
    h_freq <- hist(dif, breaks=50, col="blue", xlab="Model - Obs (C)", main='Histogram of residuals',probability = F, xlim = c(min(na.rm =T,  dif),max(na.rm =T,  dif)))
    yfit_freq <- yfit_density*diff(h_freq$mids[1:2])*length(dif)
    lines(xfit, yfit_freq, col="red",lty =2, lwd=2)
    mn <- round(mean(dif, na.rm =T),2)
    abline(v = mn,lty =2,lwd =2, col = 'green')
    std.dev <- round(sd(dif, na.rm =T),2)
    eqn <- bquote(Mean == .(mn) * "," ~~ S.D. == .(std.dev))
    Corner_text(eqn)

    plot(df$mod, dif, cex = 0.5, pch ='.', main = 'Residuals vs. Modelled',
         ylab = 'Residuals', xlab = 'Modelled values')
    abline( h =0, col =2, lty =2)

    plot(df[,1], dif, ylab = 'Time', xlab = 'Residuals', main = 'Residuals vs. Time', pch = '.')
    abline(h =0, lty =2, col =2)

    if(min(df[,2]) >= 0){
      df[,2] = -df[,2]
    }
    plot(dif, df[,2], ylim = range(df[,2]), ylab = 'Depth (m)', xlab = 'Residuals', main = 'Residuals vs. Depth', pch = '.')
    abline(v =0, lty =2, col =2)

    plot(df$mod, df$obs, pch ='.', main = 'Obs vs. Mod', ylab = 'Obs',
         xlab ='Mod', ylim = range(df$mod, df$obs, na.rm =T), xlim = range(df$mod, df$obs, na.rm =T))
    abline(0,1, col =2, lty =2)
    eqn <- bquote(Pear_R == .(round(stats$Pearson_r,2)) * "," ~~ var.obs == .(round(stats$Variance_obs,2)) *
                    "," ~~ var.mod == .(round(stats$Variance_mod,2)) *  "," ~~ NSE == .(round(stats$NSE,2)))
    eqn2 <- bquote(cov == .(round(stats$Covariance,2)) * "," ~~ bias == .(round(stats$Bias,2)) *
                     "," ~~ MAE == .(round(stats$MAE,2)) * "," ~~ RMSE == .(round(stats$RMSE,2)))
    Corner_text(eqn)
    Corner_text(eqn2,location = 'bottomright')

    qqnorm(dif)
    abline(0,1, lty =2, col =2)
  }else{
    #ggplot2 version - put all variables in one dataframe
    df$res <- df$mod - df$obs
    deps <- unique(df[,2])
    deps <- deps[order(deps)]
    if(length(deps) < 10){
      lgd.sz = 4
    }else{
      lgd.sz =2
    }
    df$fdepth <- factor(df[,2], levels = as.character(deps))

    mean.res = round(mean(df$res, na.rm =T),2)
    med.res = round(median(df$res, na.rm = T),2)
    std.dev = round(sd(df$res, na.rm =T), 2)
    n = nrow(df[!is.na(df$res),])
    bw = 0.2
    min.res = min(df$res, na.rm =T)
    max.res = max(df$res, na.rm =T)

    # Create text to be added to plots
    grob1 <- grid::grobTree(grid::textGrob(paste0("Mean = ", mean.res,'; S.D = ', std.dev), x=0.5,  y=0.95, hjust=0,
                                           gp=grid::gpar(col="black", fontsize=10)))
    grob2 <- grid::grobTree(grid::textGrob(paste0("Pear_R = ", round(stats$Pearson_r,2),'; v.obs = ', round(stats$Variance_obs,2),'; v.mod = ', round(stats$Variance_mod,2),'; NSE = ',round(stats$NSE,2)), x=0.05,  y=0.95, hjust=0,
                                           gp=grid::gpar(col="black", fontsize=10)))
    grob3 <- grid::grobTree(grid::textGrob(paste0("cov = ", round(stats$Covariance,2),'; bias = ', round(stats$Bias,2),'; MAE = ', round(stats$MAE,2),'; RMSE = ',round(stats$RMSE,2)), x=0.05,  y=0.05, hjust=0,
                                           gp=grid::gpar(col="black", fontsize=10)))


    #Plots
    p1 <-ggplot(df, aes(x = res)) +
      geom_histogram(fill = "blue", colour = 'black', breaks = seq(min.res, max.res, bw)) +
      stat_function(
        fun = function(x, mean, sd, n, bw){
          dnorm(x = x, mean = mean, sd = sd) * n * bw
        },
        args = c(mean = 0, sd = std.dev, n = n, bw = bw), colour = 'red', linetype = 'dashed', size = 1.2) +
      scale_x_continuous("Model - Obs (C)")+
      scale_y_continuous("Frequency")+
      ggtitle('Histogram of residuals')+
      coord_cartesian(xlim = c(min(df$res, na.rm = T),max(df$res,na.rm =T)))+
      geom_vline(xintercept = med.res, colour = 'green', linetype = 'dashed', size = 1.2)+
      theme_bw()
    p1 <- p1 + annotation_custom(grob1)

    p2 <- ggplot(df, aes_string('mod', 'res', colour = 'fdepth'))+
      geom_point(size = size)+
      xlab('Modelled values')+
      ylab('Residuals')+
      ggtitle('Residuals vs. Modelled')+
      {if(colourblind)scale_colour_manual(values = dramp(ndep))}+
      guides(colour = F)+
      geom_hline(yintercept = 0, size = 1, linetype = 'dashed')+
      theme_bw()

    p3 <- ggplot(df, aes_string(names(df)[1], 'res', colour = 'fdepth'))+
      geom_point(size = size)+
      xlab('Time')+
      ylab('Residuals')+
      ggtitle('Residuals vs. Time')+
      #scale_color_gradientn(colors = rev(my.cols), name = 'Depths')+
      {if(colourblind)scale_colour_manual(values = dramp(ndep))}+
      guides(colour = F)+
      geom_hline(yintercept = 0, size = 1, linetype = 'dashed')+
      theme_bw()#+
    #theme(legend.text=element_text(size= (lgd.sz*2.5)))

    p4 <- ggplot(df, aes_string('res', names(df)[2], colour = 'fdepth'))+
      geom_point(size = size)+
      ylab('Depth')+
      xlab('Residuals')+
      ggtitle('Residuals vs. Depth')+
      geom_vline(xintercept = 0, linetype = 'dashed', size = 1)+
      {if(colourblind)scale_colour_manual(values = dramp(ndep))}+
      guides(colour = F)+
      theme_bw()


    p5 <- ggplot(df,aes_string('mod', 'obs', colour = 'fdepth'))+
      geom_point(size = size)+
      ylab('Obs')+
      xlab('Modelled')+
      ggtitle('Obs vs. Mod')+
      coord_cartesian(xlim = range(df$mod, df$obs, na.rm =T), ylim = range(df$mod, df$obs, na.rm =T))+
      geom_abline(slope = 1, intercept = 0, colour = 'black', linetype = 'dashed', size =1)+
      {if(colourblind)scale_colour_manual(values = dramp(ndep))}+
      guides(colour = F)+
      theme_bw()
    p5 <- p5 + annotation_custom(grob2) + annotation_custom(grob3)

    p6 <- ggplot(df, aes(sample = res))+
      stat_qq()+
      geom_abline(slope = 1, intercept = 0, size =1, linetype = 'dashed')+
      xlab('Sample Quantiles')+
      ylab('Theoretical Quantiles')+
      ggtitle('Normal Q-Q Plot')+
      theme_bw()

    g <- gridExtra::arrangeGrob(p1,p2,p3,p4,p5,p6, nrow = 2)
    gridExtra::grid.arrange(g)

    return(g)
  }
}
