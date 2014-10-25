## Similar idea to denstrip::denstrip, but allows density to change over strip.

##' Adds a normal density strip around E(y) to existing 2D or 3D plot of y vs x.
##'
##' Density built up by layering \code{npoly} polygons.
##' @title Add Normal Density Strip
##' @param x x values (minimum 3 to enable clipping).
##' @param yhat fitted values, i.e. E(y).
##' @param sigma standard deviation of density.
##' @param npoly number of polygons.
##' @param perc middle percentage of density shown.
##' @param persp the output from \code{persp} to transform to 3D.
##' @param xlim x limits to clip to in 3D case.
##' @param ylim y limits to clip to in 3D case.
##' @param col colour of the density polygons.
##' @author Heather Turner
##' @export
##' @importFrom scales alpha
denStrip <- function(x, yhat, sigma, npoly = 50, perc = 95,
                     persp = NULL, xlim = NULL, ylim = NULL,
                     col = "red") {
    alpha <- (100 - perc)/200
    lwr <- exp(seq(log(alpha), log(0.5), length.out = npoly + 1))
    ## draw polygon from lwr[i]'th to (1 - lwr[i])'th quantile
    ## with alpha density given by density at (lwr[i] + lwr[i + 1])/2
    hlast <- 0
    for (i in seq_len(npoly)) {
        q <- c(lwr = lwr[i], upr = 1 - lwr[i], mid = mean(lwr[i:(i + 1)]))
        q <- sapply(q, qnorm, yhat, sigma)
        ## standardise so max density = 1
        ## !! probably need to change for heteroscedastic case !!
        h <- dnorm(q[, "mid"], yhat, sigma)/dnorm(yhat, yhat, sigma)
        ## composite alpha will be h^p
        p <- 1
        a <- (h^p - hlast^p)/(1 - hlast^p)
        coord <- list(x = c(x, rev(x)), y = c(q[, "lwr"], rev(q[, "upr"])))
        ## clip to plotting region (only really needed for 3D case)
        if (!is.null(ylim)) {
            too.low <- which(coord$y < ylim[1])
            if (length(too.low)) {
                ## shift last or next point to corner
                id <- max(too.low) + 1 * (length(too.low) == 1)
                coord$x[id] <- with(coord, x[id - 1] + (ylim[1] - y[id - 1]) *
                                    (x[id] - x[id - 1])/(y[id] - y[id - 1]))
                if (coord$x[id] > xlim[2]) coord$x[id] <- xlim[2]
                coord$y[c(too.low, id)] <- ylim[1]
            }
            too.high <- which(coord$y > ylim[2])
            if (length(too.high)) {
                ## shift last or next point to corner
                id <- max(too.high) + 1 * (length(too.high) == 1)
                coord$x[id] <- with(coord, x[id] + (ylim[2] - y[id]) *
                                    (x[id - 1] - x[id])/(y[id - 1] - y[id]))
                if (coord$x[id] < xlim[1]) coord$x[id] <- xlim[1]
                coord$y[c(too.high, id)] <- ylim[2]
            }
        }
        ## transform 2D coords to 3D
        if (!is.null(persp)) coord <- trans3d(coord$x, coord$y, 0, persp)
        ## plot polygon and save current total density
        polygon(coord$x, coord$y, border = NA, col = alpha(col, a))
        hlast <- h
    }
}
