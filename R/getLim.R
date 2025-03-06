##'
##'
##'
##' Get regular axis limits for given variable, i.e. with par("xaxs") or
##' par("yaxs") equal to "r".
##'
##' @title Get Axis Limits
##' @param x a numeric vector.
##' @return a vector of axis limits.
##' @author Heather Turner
##' @export
getLim <- function(x) {
     xlim <- range(x)
     xlim + c(-0.04, 0.04) * diff(xlim)
 }

