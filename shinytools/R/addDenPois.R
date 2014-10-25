##' Add Poisson density curve to persp plot in z dimension.
##'
##'
##' @title Add Poisson Density Curve to persp Plot in z Dimension
##' @param x x coordinate.
##' @param mu mean of normal ditribution.
##' @param ylim limits over which to draw density
##' @param persp output of \code{\link{persp}}.
##' @param n number of points at which to evaluate density.
##' @param perc middle percentage to colour by \code{incol}.
##' @param incol colour for middle \code{perc} percent of density.
##' @param outcol colour for density outside middle \code{perc} percent.
##' @author Heather Turner
##' @seealso \code{\link{polygon}}
##' @export
addDenPois <- function(x, mu, ylim, persp, n = 50,
                       perc = 95, incol = "slateblue4", outcol = "seashell3"){
    ## use discrete steps for y so that density non-zero
    maxy <- floor(ylim[2])
    miny <- max(ceiling(ylim[1]), 0)
    step <- max(maxy %/% n, 1)
    y <- seq(miny, maxy, by = step)
    ## find density at each point
    z <- dpois(y, mu)
    z0 <- numeric(length(z))
    ## segments
    start <- trans3d(x, y, z0, persp)
    finish <- trans3d(x, y, z, persp)
    ## if outside percentile limits use outcol
    q <- c((100 - perc), (100 + perc))/200
    q <- qpois(q, mu)
    out <- as.numeric(y < q[1] | y > q[2]) + 1
    segments(start$x, start$y, finish$x, finish$y,
             col = c(incol, outcol)[out])
}
