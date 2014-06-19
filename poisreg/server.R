## load packages
library(RColorBrewer) #brewer.pal
library(scales) # dichromat_pal
library(R.utils) # sourceDirectory
library(fields) # image.plot
library(plot3D) # persp3D

## source app-specific functions
sourceDirectory('../tools', recursive = TRUE, modifiedOnly = FALSE)

## plot colours
abCol <- "black"
inputCol <- brewer.pal(8, "Dark2")[2]
resCol <- brewer.pal(4, "RdYlBu")[c(1,4)]

## server script for linear regression app
shinyServer(function(input, output, session){
    ## get data
    dat <- reactive({na.omit(
        switch(input$data,
               "cars" = cars[c("dist", "speed")]))})
    ## get model parameters
    fit <- reactive({
        dat <- dat()
        y <- names(dat)[1]
        x <- names(dat)[2]
        par <- glm.fit(model.matrix(~dat[[x]]), dat[[y]],
                       family = poisson())$coefficients
        a <- round(par[[1]], 3)
        b <- round(par[[2]], 3)
        n <- nrow(dat)
        eqn <- bquote(log(E(.(y))) == .(a) + .(b) %.% .(x))
        list(a = a, b = b, eqn = eqn)
    })
    ## define range of parameters reactively
    step <- reactive({
        fit <- fit()
        ## parameter range: +/- 20%
        aStep <- abs(fit$a) * 0.02 + 5e-3
        aRange <- c(fit$a - 10*aStep, fit$a + 10*aStep)
        bStep <- abs(fit$b) * 0.02 + 5e-3
        bRange <- c(fit$b - 10*bStep, fit$b + 10*bStep)
        list(aMin = aRange[1], aMax = aRange[2], aStep = aStep,
             bMin = bRange[1], bMax = bRange[2], bStep = bStep)
    })
    ## start intercept and slope at fitted value
    output$intercept <- renderUI({
        numericInput("a",
                     "Intercept (a)",
                     value = fit()$a,
                     min = step()$aMin,
                     max = step()$aMax,
                     step = step()$aStep)
    })
    output$slope <- renderUI({
        numericInput("b",
                     "Slope (b)",
                     value = fit()$b,
                     min = step()$bMin,
                     max = step()$bMax,
                     step = step()$bStep)
    })

    ### Fitting Process Tab ###
    output$fitPlot <- renderPlot({
        dat <- dat()
        fit <- fit()
        step <- step()
        y <- names(dat)[1]
        x <- names(dat)[2]
        ## plot data and interactive line
        par(mfrow = c(1, 2)) # layout won't work with points etc
        ## allow space for legend (is this okay for re-sizing?)
        old.par <- par(no.readonly = TRUE)
        bigplot <- c(0.1211077, 0.7767385, 0.1836000, 0.8524000)
        par(plt = bigplot)
        plot(dat[[x]], dat[[y]], ylab = y, xlab = x)
        f <- function(x) exp(input$a + input$b*x)
        lim <- par()$usr
        if (!is.null(input$a)) # not evaluated at start-up
            curve(f, lim[1], lim[2], col = inputCol, add = TRUE)
        mtext(bquote(log(E(.(y))) == .(input$a) + .(input$b) %.% .(x)),
              side = 3, line = 3, col = inputCol)
        ## plus regression line if requested
        if (input$fitted) {
            f <- function(x) exp(fit$a + fit$b*x)
            curve(f, lim[1], lim[2], col = abCol, add = TRUE)
            mtext(fit$eqn, side = 3, line = 1, col = abCol)
        }
        ## plus residuals if requested
        if (input$residuals) {
            mu <- exp(input$a + input$b * dat[[x]])
            res <- dat[[y]] - mu
            if (input$type %in% c("pearson", "pearson2")){
                v <- 1/sqrt(poisson()$variance(mu))
            } else {
                wt <- rep.int(1, length(y))
                d <- sqrt(pmax((poisson()$dev.resids)(dat[[y]], mu, wt), 0))
                v <- sign(res) * d/res
            }
            ## convert v to colour (rescale to fill 0-1 range)
            v <- (v-min(v))/(max(v) - min(v))
            col <- colorRamp(brewer.pal(9, "PuRd"))(v)
            col <- rgb(col, maxColorValue = 255)
            if (input$type %in% c("pearson", "dev")) {
                segments(x0 = dat[[x]], y0 = dat[[y]], x1 = dat[[x]], y1 = mu,
                         col = col, lty = ifelse(sign(res) == 1, 1, 3), lwd = 3)
            } else {
                segments(x0 = c(dat[[x]], dat[[x]],
                             dat[[x]] - res, dat[[x]] - res),
                         y0 = c(dat[[y]], mu, mu, dat[[y]]),
                         x1 = c(dat[[x]], dat[[x]] - res,
                             dat[[x]] - res, dat[[x]]),
                         y1 = c(mu, mu, dat[[y]], dat[[y]]),
                         col = col, lty = ifelse(sign(res) == 1, 1, 3), lwd = 3)
            }
            image.plot(zlim = c(0, 1), col = rev(col), legend.only = TRUE,
                       legend.width = 1, smallplot= c(.80,.85,0.25,0.8),
                       legend.lab = "relative weight", legend.line = 2.5)
        }
        ## plot deviance function
        dev <- function(par, y, x) {
            wt <- rep.int(1, length(y))
            mu <- exp(par[1] + par[2] * x)
            sum(poisson()$dev.resids(y, mu, wt))
        }
        par(plt = old.par$plt)
        plotLoss(input$loss, dev,
                 seq(step$aMin - step$aStep, step$aMax + step$aStep,
                     length = 100),
                 seq(step$bMin - step$bStep, step$bMax + step$bStep,
                     length = 100), dat[[y]], dat[[x]])
        points(input$a, input$b, col = inputCol, pch = 16, cex = 1.5)
        mtext("Deviance", side = 3, line = 2, col = "black")
        if (input$fitted) {
            points(fit$a, fit$b, col = abCol, pch = 16, cex = 1.5)
            #mtext(bquote(paste(.(lossEqn), " = ",
            #                   .(lossFn(c(fit$a, fit$b), dat[[y]], dat[[x]])))),
            #      side = 3, line = 0, col = abCol)
        }
    })


    ### Fitted Distribution tab ###
     output$distPlot <- renderPlot({
        dat <- dat()
        fit <- fit()
        y <- names(dat)[1]
        x <- names(dat)[2]
        ## if adding density generate fitted data
        if (input$density2D | input$density3D) {
            ## need several points for non-gaussian - curves vs lines
            xlim <- getLim(dat[[x]]) # par()$usr no good for 3D
            ylim <- getLim(dat[[y]])
            x2D <- seq(xlim[1], xlim[2], length.out = 30)
            y2D <- exp(fit$a + fit$b * x2D)
        }
        if (!input$density3D) {
            ## set up 2D plot
            par(mar = c(5, 4, 4, 2) + 0.1)
            plot(dat[[x]], dat[[y]], ylab = y, xlab = x, type = "n")
            ## add 2D density if requested
            if (input$density2D) {
                perc <- as.numeric(gsub("%", "", input$limits))
                densStripPois(fit = fit, perc = perc, xlim = xlim, ylim = ylim)
                box()
            }
            ## add points and fitted line
            points(dat[[x]], dat[[y]])
            f <- function(x) exp(fit$a + fit$b*x)
            lim <- par()$usr
            curve(f, lim[1], lim[2], col = abCol, add = TRUE)
            ## add title
            mtext(fit$eqn, side = 3, line = 1, col = abCol)
        } else {
            ## find maximum density
            miny <- min(y2D)
            if (miny < ylim[1]) miny <- ylim[1]
            zmax <- dpois(floor(miny), miny)
            zlim <- c(0, max(zmax, 0.5)) # don't expand z range
            ## set up 3D plot
            par(mar = c(0, 2, 0, 0), xpd = TRUE)
            P <- persp3D(xlim, ylim, matrix(0, 2, 2), zlim = zlim,
                         theta = -30, phi = 15, box = FALSE, colkey = FALSE)
            perspAxis(1:2, P, xlim, ylim, zlim)
            perspLab(P, xlim, ylim, zlim, xlab = x, ylab = y)
            base <- function(){
                lines(trans3d(xlim[1], ylim, zlim[1], P))
                lines(trans3d(xlim[2], ylim, zlim[1], P))
                lines(trans3d(xlim, ylim[1], zlim[1], P))
                lines(trans3d(xlim, ylim[2], zlim[1], P))
            }
            ## add 2D density if requested
            if (input$density2D) {
                perc <- as.numeric(gsub("%", "", input$limits))
                densStripPois(fit = fit, perc = perc, persp = P,
                              xlim = xlim, ylim = ylim)
            }
            base()
            ## add points and fitted line
            points(trans3d(dat[[x]], dat[[y]], 0, P))
            if (any(too.high <- y2D > ylim[2])) {
                y2D[too.high] <- ylim[2]
                x2D[too.high] <- (ylim[2] - fit$a)/fit$b
            }
            if (any(too.low <- y2D < ylim[1])) {
                y2D[too.low] <- ylim[1]
                x2D[too.low] <- (ylim[1] - fit$a)/fit$b
            }
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
            text(trans3d(xlim[1] + diff(xlim)/2, ylim[1] + diff(ylim)/2,
                         zlim[2], P), labels = fit$eqn)
        }
    })
})
