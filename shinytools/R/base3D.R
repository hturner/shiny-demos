##' Add box to base of 3D plot.
##'
##'
##' @title  Add Box to Base of 3D Plot
##' @param xlim x limits.
##' @param ylim y limits.
##' @param zlim z limits.
##' @param P output of \code{\link{persp}}.
##' @author Heather Turner
##' @export
base3D <- function(xlim, ylim, zlim, P){
    lines(trans3d(xlim[1], ylim, zlim[1], P))
    lines(trans3d(xlim[2], ylim, zlim[1], P))
    lines(trans3d(xlim, ylim[1], zlim[1], P))
    lines(trans3d(xlim, ylim[2], zlim[1], P))
}
