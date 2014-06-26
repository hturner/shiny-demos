## add density curve to persp plot in z dimension
addDen <- function(x, # x position
                   mu, # mean
                   sigma, # sd
                   ylim, # limits over which to draw density
                   persp, # output of persp
                   n = 50, # number of points at which to evaluate density
                   perc = 95,
                   incol = "light blue",
                   outcol = "blue",
                   density = 40 # density of hashing
                   ){
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
