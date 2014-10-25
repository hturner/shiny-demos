## load packages
library(RColorBrewer) #brewer.pal
library(scales) # dichromat_pal
library(shinytools)

## plot colours
abCol <- "black"
inputCol <- brewer.pal(8, "Dark2")[2]
resCol <- brewer.pal(4, "RdYlBu")[c(1,4)]
densCol <- dichromat_pal("BluetoOrangeRed.14")(14)[c(1, 12, 14)]

## server script for linear regression app
shinyServer(function(input, output, session){
    ## get data
    dat <- reactive({na.omit(
        switch(input$data,
               "aids" = data.frame( ## Dobson 1990
                   y = c(0:3, 1, 4, 9, 18, 23, 31, 20, 25, 37, 45),
                   x = 1:14),
               "cars" = cars[c("dist", "speed")]))})
    ## get model parameters
    fit <- reactive({
        dat <- dat()
        y <- names(dat)[1]
        x <- names(dat)[2]
        family <- get(input$family)(link = input$link)
        mod <- tryCatch(glm.fit(model.matrix(~dat[[x]]), dat[[y]],
                                family = family, start = c(0, 0.5)),
                        warning = function(w) w)
        if (inherits(mod, "warning")) stop("Could not fit model", call. = FALSE)
        a <- round(coef(mod)[[1]], 3)
        b <- round(coef(mod)[[2]], 3)
        n <- nrow(dat)
        if (input$family == "gaussian") {
            sigma <- sqrt(sum((mod$weights * mod$residuals^2))/mod$df.residual)
        } else sigma <- NULL
        eqn <- bquote(log(E(.(y))) == .(a) + .(b) %.% .(x))
        list(a = a, b = b, sigma = sigma, eqn = eqn, family = family)
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
        f <- function(x) fit$family$linkinv(input$a + input$b*x)
        lim <- par()$usr
        if (!is.null(input$a)) # not evaluated at start-up
            curve(f, lim[1], lim[2], col = inputCol, add = TRUE)
        mtext(bquote(log(E(.(y))) == .(input$a) + .(input$b) %.% .(x)),
              side = 3, line = 3, col = inputCol)
        ## plus regression line if requested
        if (input$fitted) {
            f <- function(x) fit$family$linkinv(fit$a + fit$b*x)
            curve(f, lim[1], lim[2], col = abCol, add = TRUE)
            mtext(fit$eqn, side = 3, line = 1, col = abCol)
        }
        ## plus residuals if requested
        if (input$residuals) {
            mu <- fit$family$linkinv(input$a + input$b * dat[[x]])
            res <- dat[[y]] - mu
            signRes <- sign(res)
            if (input$type == "pearson"){
                res <- res/sqrt(fit$family$variance(mu))
            } else {
                wt <- rep.int(1, length(y))
                d <- sqrt(pmax((fit$family$dev.resids)(dat[[y]], mu, wt), 0))
                res <- signRes * d
            }
            radius <- sqrt(abs(res)/pi)
            symbols(dat[[x]], dat[[y]], circles = radius, inches = 0.3,
                    fg = "white",
                    bg = c(rgb(1,0,0,0.5), rgb(0,0,1,0.5))[signRes/2 + 1.5],
                    add = TRUE)
        }
        ## plot deviance function
        dev <- function(par, y, x) {
            wt <- rep.int(1, length(y))
            mu <- fit$family$linkinv(par[1] + par[2] * x)
            sum(fit$family$dev.resids(y, mu, wt))
        }
        par(plt = old.par$plt)
        plotLoss(dev,
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
            y2D <- fit$family$linkinv(fit$a + fit$b * x2D)
        }
        if (!input$density3D) {
            ## set up 2D plot
            par(mar = c(5, 4, 4, 2) + 0.1)
            plot(dat[[x]], dat[[y]], ylab = y, xlab = x, type = "n")
            ## add 2D density if requested
            if (input$density2D) {
                perc <- as.numeric(gsub("%", "", input$limits))
                if (input$family == "gaussian") {
                    denStrip(x2D, y2D, fit$sigma, perc = perc, col = densCol[1])
                } else {
                    densStripPois(fit = fit, perc = perc, xlim = xlim,
                                  ylim = ylim, col = densCol[1])
                }
                box()
            }
            ## add points and fitted line
            points(dat[[x]], dat[[y]])
            f <- function(x) fit$family$linkinv(fit$a + fit$b*x)
            lim <- par()$usr
            curve(f, lim[1], lim[2], col = abCol, add = TRUE)
            ## add title
            mtext(fit$eqn, side = 3, line = 1, col = abCol)
        } else {
            ## find maximum density
            miny <- min(y2D)
            if (miny < ylim[1]) miny <- ylim[1]
            if (input$family == "gaussian") {
                zmax <- dnorm(y2D[1], y2D[1], fit$sigma)
                zlim <- c(0, zmax*1.1)
            } else {
                zmax <- dpois(floor(miny), miny)
                zlim <- c(0, zmax) # don't expand z range
            }
            ## set up 3D plot
            par(mar = c(0, 2, 0, 0), xpd = TRUE, plt = c(0, 1, 0, 1))
            P <- persp3D(xlim, ylim, matrix(0, 2, 2), zlim = zlim,
                         theta = -30, phi = 15, box = FALSE, colkey = FALSE)
            perspAxis(1:2, P, xlim, ylim, zlim)
            perspLab(P, xlim, ylim, zlim, xlab = x, ylab = y)
            ## add 2D density if requested
            if (input$density2D) {
                perc <- as.numeric(gsub("%", "", input$limits))
                if (input$family == "gaussian") {
                    denStrip(x2D, y2D, fit$sigma, perc = perc, col = densCol[1],
                         persp = P, xlim = xlim, ylim = ylim)
                } else {
                    densStripPois(fit = fit, perc = perc, persp = P,
                                  xlim = xlim, ylim = ylim, col = densCol[1])
                }
            }
            base3D(xlim, ylim, zlim, P)
            ## add points and fitted line
            points(trans3d(dat[[x]], dat[[y]], 0, P))
            if (any(too.high <- y2D > ylim[2])) {
                y2D[too.high] <- ylim[2]
                x2D[too.high] <- (fit$family$linkfun(ylim[2]) - fit$a)/fit$b
            }
            if (any(too.low <- y2D < ylim[1])) {
                y2D[too.low] <- ylim[1]
                x2D[too.low] <- (fit$family$linkfun(ylim[1]) - fit$a)/fit$b
            }
            lines(trans3d(x2D, y2D, 0, P))
            ## add 3D densities if requested
            nq <- input$quantiles
            if (input$density3D && nq > 0) {
                perspAxis(3, P, xlim, ylim, zlim)
                perspLab(P, xlim, ylim, zlim, zlab = "Fitted Density")
                q <- seq(1/(nq + 1), nq/(nq + 1), length.out = nq)
                x3D <- xlim[1] + q * diff(range(xlim))
                y3D <- fit$family$linkinv(fit$a + fit$b * x3D)
                perc <- as.numeric(gsub("%", "", input$limits))
                for (i in seq_len(nq)) {
                    if (input$family == "gaussian") {
                        addDen(x3D[i], y3D[i], fit$sigma, ylim, P, perc = perc,
                           incol = densCol[2], outcol = densCol[3])
                    } else {
                        addDenPois(x3D[i], y3D[i], ylim, P, perc = perc,
                                   incol = densCol[2], outcol = densCol[3])
                    }
                }
            }
            text(trans3d(xlim[1] + diff(xlim)/2, ylim[1] + diff(ylim)/2,
                         zlim[2], P), labels = fit$eqn)
        }
    })
})
