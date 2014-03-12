## load packages
library(RColorBrewer) #brewer.pal
library(scales) # dichromat_pal
library(R.utils) # sourceDirectory
library(fields) #image.plot
library(ISwR) #thuesen
library(MASS) #cats

## source app-specific functions
sourceDirectory('tools', recursive = TRUE)

## plot colours
abCol <- "black"
inputCol <- brewer.pal(8, "Dark2")[2]
resCol <- brewer.pal(4, "RdYlBu")[c(1,4)]

## server script for linear regression app
shinyServer(function(input, output, session){
    ## get data
    dat <- reactive({na.omit(
        switch(input$data,
               "anscombe 1" = anscombe[c("y1", "x1")],
               "anscombe 2" = anscombe[c("y2", "x2")],
               "anscombe 3" = anscombe[c("y3", "x3")],
               "anscombe 4" = anscombe[c("y4", "x4")],
               "cars" = cars[c("dist", "speed")],
               "cats" = cats[c("Hwt", "Bwt")],
               "faithful" = faithful[c("eruptions", "waiting")],
               "thuesen" = thuesen[c("short.velocity", "blood.glucose")]))})
    ## get regression parameters
    lossFn <- reactive({
        if (input$loss == "absolute") absoluteLoss
        else quadraticLoss
    })
    fit <- reactive({
        dat <- dat()
        y <- names(dat)[1]
        x <- names(dat)[2]
        par <- optim(c(0.1, 0.1), lossFn(), method = "BFGS",
                     y = dat[[y]], x = dat[[x]])$par
        a <- round(par[[1]], 3)
        b <- round(par[[2]], 3)
        n <- nrow(dat)
        sigma <- sqrt(sum((dat[[y]] - (a + b * dat[[x]]))^2)/(n - 2))
        eqn <- bquote(E(.(y)) == .(a) + .(b) %.% .(x))
        list(a = a, b = b, sigma = sigma, eqn = eqn)
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
        lossFn <- lossFn()
        fit <- fit()
        step <- step()
        y <- names(dat)[1]
        x <- names(dat)[2]
        ## plot data and interactive line
        par(mfrow = c(1, 2)) # layout won't work with points etc
        plot(dat[[x]], dat[[y]], ylab = y, xlab = x)
        abline(input$a, input$b, col = inputCol)
        mtext(bquote(E(.(y)) == .(input$a) + .(input$b) %.% .(x)),
              side = 3, line = 3, col = inputCol)
        ## plus regression line if requested
        if (input$fitted) {
            abline(fit$a, fit$b, col = abCol)
            mtext(fit$eqn, side = 3, line = 1, col = abCol)
        }
        ## plus residuals if requested
        if (input$residuals) {
            yhat <- input$a + input$b * dat[[x]]
            res <- dat[[y]] - yhat
            segments(x0 = dat[[x]], y0 = dat[[y]], x1 = dat[[x]], y1 = yhat,
                     col = resCol[sign(res)*0.5 + 1.5])
        }
        ## plus squared residuals if requested
        if (input$squares) {
            yhat <- input$a + input$b * dat[[x]]
            res <- dat[[y]] - yhat
            segments(x0 = c(dat[[x]], dat[[x]], dat[[x]] - res, dat[[x]] - res),
                     y0 = c(dat[[y]], yhat, yhat, dat[[y]]),
                     x1 = c(dat[[x]], dat[[x]] - res, dat[[x]] - res, dat[[x]]),
                     y1 = c(yhat, yhat, dat[[y]], dat[[y]]),
                     col = resCol[sign(res)*0.5 + 1.5])
        }
        plotLoss(input$loss, lossFn,
                 seq(step$aMin - step$aStep, step$aMax + step$aStep,
                     length = 100),
                 seq(step$bMin - step$bStep, step$bMax + step$bStep,
                     length = 100), dat[[y]], dat[[x]])
        points(input$a, input$b, col = inputCol, pch = 16, cex = 1.5)
        if (input$loss == "absolute") {
            lossEqn <-  eval(parse(text = paste0("quote(sum(group(\"|\", ",
                                       y, "[i] - a - b %.% ", x,
                                       "[i], \"|\"), i, NULL))")))
        } else {
            lossEqn <- eval(parse(text = paste0("quote(sum((", y,
                                      "[i] - a - b %.% ", x,
                                      "[i])^2, i, NULL))")))
        }
        mtext(bquote(paste(.(lossEqn), " = ",
                           .(lossFn(c(input$a, input$b), dat[[y]], dat[[x]])))),
              side = 3, line = 2, col = inputCol)
        if (input$fitted) {
            points(fit$a, fit$b, col = abCol, pch = 16, cex = 1.5)
            mtext(bquote(paste(.(lossEqn), " = ",
                               .(lossFn(c(fit$a, fit$b), dat[[y]], dat[[x]])))),
                  side = 3, line = 0, col = abCol)
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
            ## add title
            mtext(fit$eqn, side = 3, line = 1, col = abCol)
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
                         persp = P, xlim = xlim, ylim = ylim)
            }
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
