library(RColorBrewer) # brewer.pal
library(R.utils) # sourceDirectory

## source additional functions
sourceDirectory('tools', recursive = TRUE)

## server script
shinyServer(function(input, output, session){
    ## colours
    abCol <- "black"
    inputCol <- brewer.pal(8, "Dark2")[2]
    resCol <- brewer.pal(4, "RdYlBu")[c(1,4)]
    ## define loss function
    lossFn <- function(par, y, x) {
        sum((y - par[1] - par[2] * x)^2)
    }
    ## get data
    dat <- reactive({switch(input$data,
                            "anscombe 1" = na.omit(anscombe[c("y1", "x1")]))})
    ## get regression parameters
    fit <- reactive({
        dat <- dat()
        y <- names(dat)[1]
        x <- names(dat)[2]
        par <- optim(c(0.1, 0.1), lossFn, method = "BFGS",
                     y = dat[[y]], x = dat[[x]])$par
        a <- round(par[[1]], 3)
        b <- round(par[[2]], 3)
        n <- nrow(dat)
        sigma <- sum((dat[[y]] - (a + b * dat[[x]]))^2)/(n - 1)
        eqn <- bquote(E(.(y)) == .(a) + .(b) %.% .(x))
        list(a = a, b = b, sigma = sigma, eqn = eqn)
    })
    ## create plot
    output$distPlot <- renderPlot({
        dat <- dat()
        fit <- fit()
        y <- names(dat)[1]
        x <- names(dat)[2]
        ## if adding density generate fitted data
        if (input$density2D | input$density3D) {
            ## need at least 3 points to enable clipping in 3D perspective
            xlim <- getLim(dat[[x]])
            x2D <- seq(xlim[1], xlim[2], length.out = 3)
            y2D <- fit$a + fit$b * x2D
        }
        if (!input$density3D) {
            ## set up 2D plot
            par(mar = c(5, 4, 4, 2) + 0.1)
            plot(dat[[x]], dat[[y]], ylab = y, xlab = x, type = "n")
            ## add 2D density if requested
            if (input$density2D) {
                perc <- as.numeric(gsub("%", "", input$limits))
                denStrip(x2D, y2D, fit$sigma, perc = perc, col = "red")
            }
            ## add points and fitted line
            points(dat[[x]], dat[[y]])
            abline(fit$a, fit$b, col = abCol)
        } else {
            xlim <- getLim(dat[[x]])
            ylim <- getLim(dat[[y]])
            ## as constant variance, all densities the same
            ## (for increasing var would need to compute for y3D at max quantile)
            zmax <- dnorm(y2D[1], y2D[1], fit$sigma)
            zlim <- c(0, max(zmax, 0.5)) # don't expand z range
            ## set up 3D plot
            par(mar = c(0, 2, 0, 0), xpd = TRUE)
            P <- persp(xlim, ylim, matrix(0, 2, 2), zlim = zlim,
                       theta = -30, box = FALSE)
            perspAxis(1:2, P, xlim, ylim, zlim)
            perspLab(P, xlim, ylim, zlim, xlab = x, ylab = y)
            ## add 2D density if requested
            if (input$density2D) {
                perc <- as.numeric(gsub("%", "", input$limits))
                denStrip(x2D, y2D, fit$sigma, perc = perc, col = "red",
                         persp = P, ylim = ylim)
            }
            ## add points and fitted line
            points(trans3d(dat[[x]], dat[[y]], 0, P))
            lines(trans3d(x2D, y2D, 0, P))
            ## add 3D densities if requested
            nq <- input$quantiles
            if (input$density3D && nq > 0) {
                perspAxis(3, P, xlim, ylim, zlim)
                perspLab(P, xlim, ylim, zlim, zlab = "Fitted Density")
                q <- seq(1/(nq + 1), nq/(nq + 1), length.out = nq)
                x3D <- xlim[1] + q * diff(range(xlim))
                y3D <- fit$a + fit$b * x3D
                for (i in seq_len(nq)) {
                    addDen(x3D[i], y3D[i], fit$sigma, ylim, P)
                }
            }
        }
        ## add title
        mtext(fit$eqn, side = 3, line = 1, col = abCol)
    })
})
