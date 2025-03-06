##' Add normal density curve to persp plot in z dimension.
##'
##'
##' @title Add Normal Density Curve to persp Plot in z Dimension
##' @param x x coordinate.
##' @param mu mean of normal ditribution.
##' @param sigma standard deviation of normal distribution.
##' @param ylim limits over which to draw density
##' @param persp output of \code{\link{persp}}.
##' @param n number of points at which to evaluate density.
##' @param perc middle percentage to colour by \code{incol}.
##' @param incol colour for middle \code{perc} percent of density.
##' @param outcol colour for density outside middle \code{perc} percent.
##' @param density density of hashing.
##' @author Heather Turner
##' @seealso \code{\link{polygon}}
##' @export
##' @importFrom scales alpha
addDen <- function(x, mu, sigma, ylim, persp, n = 50,
                   perc = 95, incol = "light blue", outcol = "blue",
                   density = 40){
    ## sequence along ylim
    y <- seq(ylim[1], ylim[2], length = n)
    ## add in percentage points
    q <- c((100 - perc), (100 + perc))/200
    q <- qnorm(q, mu, sigma)
    q[1] <- max(q[1], min(y))
    q[2] <- min(q[2], max(y))
    y <- sort(c(y, q))
    ## density
    z <- dnorm(y, mu, sigma)
    z0 <- numeric(length(y))
    ## density slices - color by whether in/out middle perc%
    break1 <- which(y == q[1])[1]
    break2 <- which(y == q[2])[1]
    set <- list(1:break1, break1:break2, break2:length(y))
    for (i in 1:3) {
        coord <- trans3d(x,
                         c(y[set[[i]]], rev(y[set[[i]]])),
                         c(z[set[[i]]], z0[set[[i]]]), persp)
        col <- alpha(ifelse(i == 2, incol, outcol), 0.5)
        polygon(coord, border = col, col = col)
    }
}
