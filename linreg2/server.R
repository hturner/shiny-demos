library(RColorBrewer)
library(R.utils)
## server script
shinyServer(function(input, output, session){
    ## source additional functions
    sourceDirectory('tools', recursive = TRUE)
    ## create plot
    output$distPlot <- renderPlot({
        ## colours
        abCol <- "black"
        inputCol <- brewer.pal(8, "Dark2")[2]
        resCol <- brewer.pal(4, "RdYlBu")[c(1,4)]
        ## define loss function
        lossFn <- function(par) {
            sum((y - par[1] - par[2] * x)^2)
        }
        ## get y
        y <- switch(input$data,
                    "anscombe 1" = anscombe[["y1"]])
        ylab <- switch(input$data,
                       "y")
        ## get x
        x <- switch(input$data,
                    "anscombe 1" = anscombe[["x1"]],)
        xlab <- switch(input$data,
                       "x")
        na <- is.na(y) | is.na(x)
        y <- y[!na]
        x <- x[!na]
        ## get fit
        par <- optim(c(0.1, 0.1), lossFn, "BFGS")$par
        a <- round(par[[1]], 3)
        b <- round(par[[2]], 3)
        n <- length(y)
        sigma <- sum((y - (a + b * x))^2)/(n - 1)
        ## if adding density generate fitted data
        if (input$density2D | input$density3D) {
            ## need at least 3 points to enable clipping in 3D perspective
            xlim <- getLim(x)
            x2D <- seq(xlim[1], xlim[2], length.out = 3)
            y2D <- a + b * x2D
        }
        if (!input$density3D) {
            ## set up 2D plot
            plot(y ~ x, ylab = ylab, xlab = xlab, type = "n")
            ## add 2D density if requested
            if (input$density2D) {
                perc <- as.numeric(gsub("%", "", input$limits))
                denStrip(x2D, y2D, sigma, perc = perc, col = "red")
            }
            ## add points and fitted line
            points(x, y)
            abline(a, b, col = abCol)
        } else {
            xlim <- getLim(x)
            ylim <- getLim(y)
            ## as constant variance, all densities the same
            ## (for increasing var would need to compute for y3D at max quantile)
            zmax <- dnorm(y2D[1], y2D[1], sigma)
            zlim <- c(0, max(zmax, 0.5)) # don't expand z range
            ## set up 3D plot
            P <- persp(xlim, ylim, matrix(0, 2, 2), zlim = zlim,
                       theta = -30, box = FALSE)
            perspAxis(1:2, P, xlim, ylim, zlim)
            ## add 2D density if requested
            if (input$density2D) {
                perc <- as.numeric(gsub("%", "", input$limits))
                denStrip(x2D, y2D, sigma, perc = perc, col = "red",
                         persp = P, ylim = ylim)
            }
            ## add points and fitted line
            points(trans3d(x, y, 0, P))
            lines(trans3d(x2D, y2D, 0, P))
            ## add 3D densities if requested
            nq <- input$quantiles
            if (input$density3D && nq > 0) {
                perspAxis(3, P, xlim, ylim, zlim)
                q <- seq(1/(nq + 1), nq/(nq + 1), length.out = nq)
                x3D <- xlim[1] + q * diff(range(xlim))
                y3D <- a + b * x3D
                for (i in seq_len(nq)) {
                    addDen(x3D[i], y3D[i], sigma, ylim, P)
                }
            }
        }
        ## add title
        mtext(bquote(E(.(ylab)) == .(a) + .(b) %.% .(xlab)),
                  side = 3, line = 1, col = abCol)
    })
})
