##' Add axes to persp plot.
##'
##'
##' @title Add Axes to persp Plot
##' @param axis vector specifying one or more axes to add.
##' @param persp the output from \code{persp} to transform to 3D.
##' @param xlim x limits.
##' @param ylim y limits.
##' @param zlim z limits.
##' @author Heather Turner
##' @export
perspAxis <- function(axis = 1:3, #1 = x, 2 = y, 3 = z
                      persp, xlim, ylim, zlim){
    if (1 %in% axis) {
        ticks <- getTicks(xlim)
        len <- diff(ylim) * 0.03
        xy0 <- trans3d(ticks, ylim[1], zlim[1], persp)
        xy1 <- trans3d(ticks, ylim[1]- len, zlim[1], persp)
        segments(xy0$x, xy0$y, xy1$x, xy1$y)
        xy2 <- trans3d(ticks, ylim[1]- 2*len, zlim[1], persp)
        text(xy2$x, xy2$y, labels = ticks, adj = c(0.5, 0.5))
    }
    if (2 %in% axis) {
        ticks <- getTicks(ylim)
        len <- diff(xlim) * 0.03
        xy0 <- trans3d(xlim[1], ticks, zlim[1], persp)
        xy1 <- trans3d(xlim[1] - len, ticks, zlim[1], persp)
        segments(xy0$x, xy0$y, xy1$x, xy1$y)
        xy2 <- trans3d(xlim[1]- 2*len, ticks, zlim[1], persp)
        text(xy2$x, xy2$y, labels = ticks, adj = c(1, 0.5))
    }
    if (3 %in% axis) {
        ticks <- getTicks(zlim)
        len <- diff(xlim) * 0.03
        xy0 <- trans3d(xlim[1], ylim[2], ticks, persp)
        xy1 <- trans3d(xlim[1] - len, ylim[2], ticks, persp)
        lines(xy0)
        segments(xy0$x, xy0$y, xy1$x, xy1$y)
        xy2 <- trans3d(xlim[1]- 2*len, ylim[2], ticks, persp)
        text(xy2$x, xy2$y, labels = ticks, adj = c(1, 0.5))
    }
}

getTicks <- function(lim) {
    ticks <- pretty(lim)
    ticks[ticks >= lim[1] & ticks <= lim[2]]
}
