## add density curve to persp plot in z dimension
addDen <- function(x, # x position
                   mu, # mean
                   sigma, # sd
                   ylim, # limits over which to draw density
                   persp, # output of persp
                   n = 50, # number of points at which to evaluate density
                   fillcol = "light blue",
                   topcol = "blue",
                   density = 40 # density of hashing
                   ){
    y <- seq(ylim[1], ylim[2], length = n)
    z <- dnorm(y, mu, sigma)
    z0 <- numeric(n)
    coord <- trans3d(x, c(y, rev(y)), c(z, z0), persp)
    ## density slice
    polygon(coord, border = NA, col = fillcol, density = density)
    ## top line
    lines(lapply(coord, "[", 1:n), col = topcol)
    ## bottom line
    lines(lapply(coord, "[", -(1:n)), lty = 2)
}
