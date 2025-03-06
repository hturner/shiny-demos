##' Add axis labels to persp plot.
##'
##'
##' @title Add Axis Labels to persp Plot
##' @param persp the output from \code{persp} to transform to 3D.
##' @param xlim x limits.
##' @param ylim y limits.
##' @param zlim z limits.
##' @param xlab label for x axis.
##' @param ylab label for y axis.
##' @param zlab label for z axis.
##' @param ... further arguments passed to \code{text}.
##' @references  Based on \url{http://rwiki.sciviews.org/doku.php?id=tips:graphics-3d:zlabspace}.
##' @author Heather Turner
##' @export
perspLab <- function(persp,
                     xlim = NULL,
                     ylim = NULL,
                     zlim = NULL,
                     xlab = NULL,
                     ylab = NULL,
                     zlab = NULL,
                     ...){

    if (is.null(xlim) | is.null(ylim) | is.null(zlim))
        stop("xlim, ylim and zlim must all be specified")

    if (!is.null(xlab)){
        len <- diff(ylim) * 0.03
        ## go out 3 * len
        start <- trans3d(xlim[1], ylim[1], zlim[1], persp)
        end <- trans3d(xlim[1], ylim[1] - 3*len, zlim[1], persp)
        ## find angle of projection
        phi <- atan2(start$y - end$y, start$x - end$x)
        ## keep going enough to cover height of highest x label
        ticks <- getTicks(xlim)
        h <- max(sapply(ticks, "strheight"))
        w <- h/tan(phi)
        ## same at top
        start2 <- trans3d(xlim[2], ylim[1], zlim[1], persp)
        end2 <- trans3d(xlim[2], ylim[1] - 3*len, zlim[1], persp)
        phi2 <- atan2(start2$y - end2$y, start2$x - end2$x)
        w2 <- h/tan(phi2)
        ## find mid-point between two end points
        A <- (end2$y - h) - (end$y - h)
        B <- (end2$x - w2) - (end$x - w)
        ## and angle at distance half-way out
        d <- sqrt((start$y - (end$y - h))^2 + (start$x - (end$x - w))^2)/2
        x <- start$x - d * cos(phi)
        y <- start$y - d * sin(phi)
        d2 <- sqrt((start2$y - (end2$y - h))^2 + (start2$x - (end2$x - w2))^2)/2
        x2 <- start2$x - d2 * cos(phi2)
        y2 <- start2$y - d2 * sin(phi2)
        ang <- 180/pi *  atan2(y - y2, x - x2)
        if (abs(ang) > 100 & abs(ang) < 260) ang <- ang + 180
        text(end$x - w + B/2, end$y - h + A/2, xlab, srt = ang, ...)
    }

    ## instead of +1 work out unit along diff(lim)*0.03
    if (!is.null(ylab)){
        len <- diff(xlim) * 0.03
        ## go out 3 * len
        start <- trans3d(xlim[1], ylim[1], zlim[1], persp)
        end <- trans3d(xlim[1] - 3*len, ylim[1], zlim[1], persp)
        ## find angle of projection
        phi <- atan2(start$y - end$y, start$x - end$x)
        ## keep going enough to cover width of widest y label
        ticks <- getTicks(ylim)
        w <- max(sapply(ticks, "strwidth"))
        h <- w * tan(phi)
        ## same at top
        start2 <- trans3d(xlim[1], ylim[2], zlim[1], persp)
        end2 <- trans3d(xlim[1] - 3*len, ylim[2], zlim[1], persp)
        phi2 <- atan2(start2$y - end2$y, start2$x - end2$x)
        h2 <- w * tan(phi2)
        ## find mid-point between two end points
        A <- (end2$y - h2) - (end$y - h)
        B <- (end2$x - w) - (end$x - w)
        ## and angle at distance half-way out
        d <- sqrt((start$y - (end$y - h))^2 + (start$x - (end$x - w))^2)/2
        x <- start$x - d * cos(phi)
        y <- start$y - d * sin(phi)
        d2 <- sqrt((start2$y - (end2$y - h2))^2 + (start2$x - (end2$x - w))^2)/2
        x2 <- start2$x - d2 * cos(phi2)
        y2 <- start2$y - d2 * sin(phi2)
        ang <- 180/pi *  atan2(y - y2, x - x2)
        if (abs(ang) > 100 & abs(ang) < 260) ang <- ang + 180
        text(end$x - w + B/2, end$y - h + A/2, ylab, srt = ang, ...)
    }

    if (!is.null(zlab)){
        len <- diff(xlim) * 0.03
        ## go out 3 * len (to test)
        start <- trans3d(xlim[1], ylim[2], zlim[1], persp)
        end <- trans3d(xlim[1] - 3*len, ylim[2], zlim[1], persp)
        ## find angle of projection
        phi <- atan2(start$y - end$y, start$x - end$x)
        ## keep going enough to cover width of widest z label
        ticks <- getTicks(zlim)
        w <- max(sapply(ticks, "strwidth"))
        h <- w * tan(phi)
        ## same at top
        start2 <- trans3d(xlim[1], ylim[2], zlim[2], persp)
        end2 <- trans3d(xlim[1] - 3*len, ylim[2], zlim[2], persp)
        phi2 <- atan2(start2$y - end2$y, start2$x - end2$x)
        h2 <- w * tan(phi2)
        ## find mid-point between two end points
        A <- (end2$y - h2) - (end$y - h)
        B <- (end2$x - w) - (end$x - w)
        ## and angle at distance half-way out
        d <- sqrt((start$y - (end$y - h))^2 + (start$x - (end$x - w))^2)/2
        x <- start$x - d * cos(phi)
        y <- start$y - d * sin(phi)
        d2 <- sqrt((start2$y - (end2$y - h2))^2 + (start2$x - (end2$x - w))^2)/2
        x2 <- start2$x - d2 * cos(phi2)
        y2 <- start2$y - d2 * sin(phi2)
        ang <- 180/pi *  atan2(y - y2, x - x2)
        if (abs(ang) > 100 & abs(ang) < 260) ang <- ang + 180
        text(end$x - w + B/2, end$y - h + A/2, zlab, srt = ang, adj = c(0.5, 1),
             ...)
    }
}

