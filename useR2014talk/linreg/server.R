## load packages
library(RColorBrewer) #brewer.pal
library(scales) # dichromat_pal
library(ISwR) #thuesen
library(MASS) #cats

## plot colours
abCol <- "black"
inputCol <- brewer.pal(8, "Dark2")[2]
resCol <- brewer.pal(4, "RdYlBu")[c(1,4)]
densCol <- dichromat_pal("BluetoOrangeRed.14")(14)[c(1, 12, 14)]

## add all code explicitly
addDen <- function(x, mu, sigma, ylim, persp, n = 50,
                   perc = 95, incol = "light blue", outcol = "blue",
                   density = 40){
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

base3D <- function(xlim, ylim, zlim, P){
    lines(trans3d(xlim[1], ylim, zlim[1], P))
    lines(trans3d(xlim[2], ylim, zlim[1], P))
    lines(trans3d(xlim, ylim[1], zlim[1], P))
    lines(trans3d(xlim, ylim[2], zlim[1], P))
}

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

getLim <- function(x) {
    xlim <- range(x)
    xlim + c(-0.04, 0.04) * diff(xlim)
}

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

plotLoss <- function(loss, a, b, y, x){
    gr <- expand.grid(a = a, b = b)
    l <- apply(gr, 1, loss, y, x)
    l <- matrix(l, length(a), length(b))
    maxLog2 <- function(x, n = 0){
        out <- suppressWarnings(log2(x))
        if (is.finite(out)){
            n <- n + 1
            Recall(out, n)
        } else n
    }
    nLog2 <- function(x, n = 1){
        out <- suppressWarnings(log2(x))
        n <- n - 1
        if (n > 0) Recall(out, n)
        else out
    }
    ticks <- signif(round(quantile(l, c(0.2, 0.4, 0.6, 0.8))), 2)
    n <- pmin(maxLog2(min(l)), 3)
    require(fields)
    image.plot(a, b, nLog2(l, n),
               col = rev(dichromat_pal("BrowntoBlue.12")(12)),
               axis.args = list(at = nLog2(ticks, n), labels=ticks),
               smallplot= c(.84,.89,0.2,0.8))
}

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
        if (input$loss == "absolute") {
            function(par, y, x) sum(abs(y - par[1] - par[2] * x))
        } else function(par, y, x) sum((y - par[1] - par[2] * x)^2)
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
    output$fitPlot <- renderPlot(height = 400, {
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
        plotLoss(lossFn,
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
                denStrip(x2D, y2D, fit$sigma, perc = perc, col = densCol[1])
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
                denStrip(x2D, y2D, fit$sigma, perc = perc, col = densCol[1],
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
                perc <- as.numeric(gsub("%", "", input$limits))
                for (i in seq_len(nq)) {
                    addDen(x3D[i], y3D[i], fit$sigma, ylim, P, perc = perc,
                           incol = densCol[2], outcol = densCol[3])
                }
            }
            text(trans3d(xlim[1] + diff(xlim)/2, ylim[1] + diff(ylim)/2,
                         zlim[2], P), labels = fit$eqn)
        }
    })
})
