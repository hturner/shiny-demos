densStripPois <- function(fit,
                          perc = 95, # middle % of density shown, in [0, 100]
                          nx = 128, # x values
                          ny = 128,
                          persp = NULL, # output from persp to transform to 3D
                          xlim = NULL,
                          ylim = NULL,
                          col = "red" # used as max colour
                          ) {
    ### split x and y into grid
    ## assume x continuous
    x2D <- seq(xlim[1], xlim[2], length.out = nx)
    ## use discrete steps for y so that density non-zero
    maxy <- floor(ylim[2])
    miny <- ceiling(ylim[1])
    step <- max(maxy %/% ny, 1)
    y2D <- seq(miny, maxy, by = step)
    grid <- expand.grid(x2D, y2D)

    ### for each grid point, predict density
    mu <- exp(fit$a + fit$b * grid[,1])
    d <- dpois(grid[,2], mu)
    ## if outside percentile limits set to zero
    q <- c((100 - perc), (100 + perc))/200
    q <- sapply(q, qpois, mu)
    zap <- grid[,2] < q[,1] | grid[,2] > q[,2]
    d[zap] <- 0

    ## transform 2D coords to 3D
    if (!is.null(persp)) {
        image3D(z = 0,
                colvar =  matrix(d, nrow = length(x2D), ncol = length(y2D)),
                xlim = xlim, x = x2D, ylim = ylim, y = y2D,
                colkey = FALSE, col = colorRampPalette(c("white", col))(256),
                add = TRUE)
    } else {
        image(x2D, y2D, matrix(d, nrow = length(x2D), ncol = length(y2D)),
              col = colorRampPalette(c("white", col))(256), add = TRUE)
    }
}
