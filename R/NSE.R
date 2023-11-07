#' Nash-Sutcliffe Efficiency
#'
#' Nash-Sutcliffe efficiencies (Nash and Sutcliffe, 1970) range from -Inf to 1.
#' An efficiency of 1 (NSE = 1) corresponds to a perfect match of modeled to the observed data.
#' An efficiency of 0 (NSE = 0) indicates that the model predictions are as accurate
#' as the mean of the observed data, whereas
#' an efficiency less than zero (-Inf < NSE < 0) occurs when the observed mean is a better predictor than the model.
#' Essentially, the closer the model efficiency is to 1, the more accurate the model is.
#'
#' @param sim  numeric 'data.frame', 'matrix' or 'vector' with simulated values
#' @param obs numeric 'data.frame', 'matrix' or 'vector' with observed values
#' @param na,rm boolean; a logical value indicating whether NA values should be stripped before the computation proceeds. Defaults to FALSE
#' @return data frame of normalized modelled depths
#' @import utils
#' @export

NSE <-function(sim, obs, na.rm = FALSE){
  denominator <- sum( (obs - mean(obs, na.rm = na.rm))^2 , na.rm = na.rm)

  if (denominator != 0) {

    NS <- 1 - ( sum( (obs - sim)^2, na.rm = na.rm) / denominator )

  } else {
    NS <- NA
    warning("'sum((obs - mean(obs))^2)=0' => it is not possible to compute 'NSE'")
  } # ELSE end

  return(NS)
}

NSE.default <- function (sim, obs, na.rm=TRUE, FUN=NULL, epsilon=c(0, "Pushpalatha2012", "other"), epsilon.value=NA, ...){

  if ( is.na(match(class(sim), c("integer", "numeric", "ts", "zoo", "xts"))) |
       is.na(match(class(obs), c("integer", "numeric", "ts", "zoo", "xts")))
  ) stop("Invalid argument type: 'sim' & 'obs' have to be of class: c('integer', 'numeric', 'ts', 'zoo', 'xts')")

  vi <- valindex(sim, obs)

  obs <- obs[vi]
  sim <- sim[vi]

  if (!is.null(FUN)) {
    new <- preproc(sim=sim, obs=obs, FUN=FUN, epsilon=epsilon, epsilon.value=epsilon.value, ...)
    sim <- new[["sim"]]
    obs <- new[["obs"]]
  } # IF end

  denominator <- sum( (obs - mean(obs))^2 )

  if (denominator != 0) {

    NS <- 1 - ( sum( (obs - sim)^2 ) / denominator )

  } else {
    NS <- NA
    warning("'sum((obs - mean(obs))^2)=0' => it is not possible to compute 'NSE'")
  } # ELSE end

  return(NS)

} # 'NSE' end


NSE.matrix <- function (sim, obs, na.rm=TRUE, FUN=NULL, epsilon=c(0, "Pushpalatha2012", "other"), epsilon.value=NA, ...){

  # Checking that 'sim' and 'obs' have the same dimensions
  if ( all.equal(dim(sim), dim(obs)) != TRUE )
    stop( paste("Invalid argument: dim(sim) != dim(obs) ( [",
                paste(dim(sim), collapse=" "), "] != [",
                paste(dim(obs), collapse=" "), "] )", sep="") )

  NS <- rep(NA, ncol(obs))

  NS <- sapply(1:ncol(obs), function(i,x,y) {
    NS[i] <- NSE.default( x[,i], y[,i], na.rm=na.rm, FUN=FUN, epsilon=epsilon, epsilon.value=epsilon.value, ...)
  }, x=sim, y=obs )

  names(NS) <- colnames(obs)

  return(NS)

} # 'NSE.matrix' end


NSE.data.frame <- function (sim, obs, na.rm=TRUE, FUN=NULL, epsilon=c(0, "Pushpalatha2012", "other"), epsilon.value=NA, ...){

  sim <- as.matrix(sim)
  obs <- as.matrix(obs)

  NSE.matrix(sim, obs, na.rm=na.rm, FUN=FUN, epsilon=epsilon, epsilon.value=epsilon.value, ...)

} # 'NSE.data.frame' end


NSeff <-function(sim, obs, ...) UseMethod("NSE")


################################################################################
# Author: Mauricio Zambrano-Bigiarini                                          #
################################################################################
# Started: 22-Mar-2013                                                         #
# Updates:                                                                     #
################################################################################
NSE.zoo <- function(sim, obs, na.rm=TRUE, FUN=NULL, epsilon=c(0, "Pushpalatha2012", "other"), epsilon.value=NA, ...){

  sim <- zoo::coredata(sim)
  if (is.zoo(obs)) obs <- zoo::coredata(obs)

  if (is.matrix(sim) | is.data.frame(sim)) {
    NSE.matrix(sim, obs, na.rm=na.rm, FUN=FUN, epsilon=epsilon, epsilon.value=epsilon.value, ...)
  } else NextMethod(sim, obs, na.rm=na.rm, FUN=FUN, epsilon=epsilon, epsilon.value=epsilon.value, ...)

} # 'NSE.zoo' end


### Helper functions from the hydroGOF package
# File preproc.R
# Part of the hydroGOF R package, https://github.com/hzambran/hydroGOF ; 
#                                 https://cran.r-project.org/package=hydroGOF
#                                 http://www.rforge.net/hydroGOF/
# Copyright 2017-2022 Mauricio Zambrano-Bigiarini
# Distributed under GPL 2 or later

################################################################################
# 'preproc': It applies a user-defined function to simulated and observed      #
#            values before computing any goodness-of-fit function, probably    #
#            adding a user-defined (and small) 'epsilon' value in order to     #
#            allow the use of logarithm and other similar functions that do    #
#            not work with zero values                                         #
################################################################################
# Author: Mauricio Zambrano-Bigiarini                                          #
################################################################################
# Reference: Pushpalatha, R., Perrin, C., Le Moine, N., & Andreassian, V.      #
#            (2012). A review of efficiency criteria suitable for evaluating   #
#            low-flow simulations. Journal of Hydrology, 420, 171-182.         #
#            DOI: 10.1016/j.jhydrol.2011.11.055                                #
################################################################################
# Started: 29-Jun-2017                                                         #
# Updates: 11-Jul-2022 ; 12-Jul-2022 ; 13-Jul-2022                             #
################################################################################
# 'sim'     : numeric, with simulated values
# 'obs'     : numeric, with observed values
# 'fun'     : function to be applied to 'sim' and 'obs' in order to obtain 
#             transformed values thereof before applying any goodness-of-fit 
#             function included in the hydroGOF package
# '...'     : additional argument to be passed to fun
# 'epsilon.type' : argument used to define a numeric value to be added to both 'sim' 
#                  and 'obs' before applying fun. It is was  designed to allow the 
#                  use of logarithm and other similar functions that do not work with 
#                  zero values. It must be one of the following three possible values:
#             -) "Pushpalatha2012": one hundredth of the mean observed values is 
#                                   added to both 'sim' and 'obs', as described  
#                                   in Pushpalatha et al., (2012). 
#             -) "otherFactor"    : the numeric value defined in the \code{epsilon.value} 
#                                   argument is used to multiply the the mean 
#                                   observed values, instead of the 
#                                   one hundredth (1/100) described in Pushpalatha et al. (2012). 
#                                   The resulting value is then added to both 
#                                   \code{sim} and \code{obs}.
#             -) "otherValue"     : the numeric value defined in the 'epsilon.value'
#                                   argument is directly added to both 'sim' and 'obs'

# 'epsilon.value': numeric value to be added to both 'sim' and 'obs' when 
#                  'epsilon="other"'

# 'Output': a list with two numeric vectors:
#           1) 'sim': simulated values after adding 'epsilon.value' and 
#                     applying 'fun' 
#           2) 'obs': observed values after adding 'epsilon.value' and 
#                     applying 'fun' 
preproc <- function (sim, obs, na.rm=TRUE, fun,  ..., 
                     epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
                     epsilon.value=0) { 
  
  # fun ?
  fun.exists <- FALSE
  if (!missing(fun)) {
    fun.exists <- TRUE
    fun        <- match.fun(fun)
  } # IF end
  
  # epsilon.type ?
  epsilon.type <- match.arg(epsilon.type)
  
  if (epsilon.type %in% c("otherFactor", "otherValue") )  {
    if (is.na(epsilon.value))
      stop("Missing argument: you need to provide 'epsilon.value' !")
    
    if ( !is.numeric(epsilon.value) )
      stop("Invalid argument: 'epsilon.value' must be numeric !")
  } # IF end
  
  # epsilon.value 
  if (epsilon.type != "none") {
    if (epsilon.type=="Pushpalatha2012") {    
      epsilon.value <- (1/100)*mean(obs, na.rm=na.rm)
    } else if (epsilon.type=="otherFactor") {
      epsilon.value <- epsilon.value*mean(obs, na.rm=na.rm)
    } # ELSE (epsilon="otherValue"): epsilon.value=epsilon.value
  } else epsilon.value <- 0
  
  # Adding epsilon, before applying fun
  obs <- obs + epsilon.value
  sim <- sim + epsilon.value
  
  # using fun (and 'epsilon.value')
  if (fun.exists) {
    obs.bak <- obs
    sim.bak <- sim
    
    obs <- fun( obs, ...)     
    sim <- fun( sim, ...)
    
    if (length(obs) != length(obs.bak))
      stop("Invalid argument: 'fun' returns an object with a length different from 'obs' or 'sim' !")
  } # IF 'fun.exists' end
  
  out <- list(sim=sim, obs=obs)
  
  return(out)
  
} # 'preproc' END

# File valindex.R
# Part of the hydroGOF R package, http://www.rforge.net/hydroGOF/ ; 
#                                 http://cran.r-project.org/web/packages/hydroGOF/
# Copyright 2011-2013 Mauricio Zambrano-Bigiarini
# Distributed under GPL 2 or later

################################################################################
# 'valindex': index of the elements that belongs to both vectors               #
################################################################################
# Author: Mauricio Zambrano-Bigiarini                                          #
################################################################################
# Started: 19-Jan-2009                                                         #
# Updates: 08-May-2012                                                         #
#          22-Mar-2013 ; 15-Apr-2013                                           #
################################################################################
# 'x'     : vector (numeric, xts, zoo)
# 'y'     : vector (numeric, xts, zoo)
# 'Result': index containing the position in 'x' and 'y' where both vectors 
#           have valid elements (NON- NA)

valindex <- function(sim, obs, ...) UseMethod("valindex")

valindex.default <- function(sim, obs, ...) {  
  
  if ( length(obs) != length(sim) ) {
    stop( "Invalid argument: 'length(sim) != length(obs)' !! (", length(sim), "!=", length(obs), ") !!" )
  } else { 
    index <- which(!is.na(sim) & !is.na(obs))
    if (length(index)==0) warning("'sim' and 'obs' are empty or they do not have any common pair of elements with data !!")
    return( index  )
  } # ELSE end
  
} # 'valindex' END


################################################################################
# Author: Mauricio Zambrano-Bigiarini                                          #
################################################################################
# Started: 25-Jul-2011                                                         #
# Updates: 08-May-2012                                                         #
################################################################################
valindex.matrix <- function(sim, obs, ...) { 
  
  # Checking that 'sim' and 'obs' have the same dimensions
  if ( all.equal(dim(sim), dim(obs)) != TRUE ) {
    stop( paste("Invalid argument: dim(sim) != dim(obs) ( [", 
                paste(dim(sim), collapse=" "), "] != [", 
                paste(dim(obs), collapse=" "), "] )", sep="") )
  } else  
    return ( !is.na( sim) & !is.na(obs) )
  
} # 'valindex.matrix' END
