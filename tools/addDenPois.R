## add poisson density curve to persp plot in z dimension
addDenPois <- function(x, # x position
                       mu, # mean
                       ylim, # limits over which to draw density
                       persp, # output of persp
                       n = 50, # number of points at which to evaluate density
                       perc = 95,
                       incol = "slateblue4",
                       outcol = "seashell3",
                       alpha = 0.4
                       ){
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
