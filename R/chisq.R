## --##--##--##--##--##--##--##--##--##--##--##--##--##--##
##   Functions related to the Chi Square distribution    ##
## --##--##--##--##--##--##--##--##--##--##--##--##--##--##

#' @importFrom stats dchisq pchisq rchisq
#' @param df degrees of freedom for "parent" distribution
#' @rdname rtrunc
#' @export
rtruncchisq <- rtrunc.chisq <- function(n, df, a = 0, b = Inf) {
  class(n) <- "trunc_chisq"
  sampleFromTruncated(mget(ls()))
}

#' @export
dtrunc.trunc_chisq <- function(y, df, eta, a = 0, b = Inf, ...) {
  if (missing(eta)) {
    eta <- parameters2natural.trunc_chisq(c("df" = df))
  }
  df <- natural2parameters.trunc_chisq(eta)
  dens <- ifelse((y <= a) | (y > b), 0, dchisq(y, df = df))
  F.a <- pchisq(a, df)
  F.b <- pchisq(b, df)
  dens <- dens / (F.b - F.a)
  attributes(dens) <- attributes(y)
  return(dens)
}

#' @rdname dtrunc
#' @export
dtruncchisq <- dtrunc.trunc_chisq

#' @export
empiricalParameters.trunc_chisq <- function(y, ...) {
  # Returns empirical parameter estimate for df
  parms <- c("df" = mean(y))
  class(parms) <- "trunc_chisq"
  return(parms)
}

sufficientT.trunc_chisq <- function(y) {
  return(suff.T = log(y))
}

#' @export
natural2parameters.trunc_chisq <- function(eta) {
  # eta: The natural parameters in a Chi Square distribution
  # returns df
  df <- c(df = 2 * (eta + 1))
  class(df) <- class(eta)
  return(df)
}

#' @export
parameters2natural.trunc_chisq <- function(parms) {
  # parms: The parameter lambda in a Chi Square distribution
  # returns the natural parameters
  eta <- prepEta(parms / 2 - 1, class(parms))
  return(eta)
}

getGradETinv.trunc_chisq <- function(eta) {
  # eta: Natural parameter
  # return the inverse of E.T differentiated with respect to eta
  return(A = 1 / sum(1 / (as.vector(eta) + (1:1e6))^2))
}

getYseq.trunc_chisq <- function(y, y.min = 0, y.max, n = 100) {
  mean <- mean(y, na.rm = TRUE)
  var.y <- var(y, na.rm = TRUE)
  lo <- max(round(y.min), 0)
  hi <- min(y.max, round(mean + 10 * sqrt(var.y)))
  out <- seq(lo, hi, length = n)
  class(out) <- class(y)
  return(out)
}
