library(RColorBrewer) #brewer.pal
library(scales) #dichromat_pal
library(fields) #image.plot
library(ISwR) #thuesen
library(MASS) #cats
## server script for linear regression app
shinyServer(function(input, output, session){
    output$distPlot <- renderPlot({
        ## colours
        abCol <- "black"
        inputCol <- brewer.pal(8, "Dark2")[2]
        resCol <- brewer.pal(4, "RdYlBu")[c(1,4)]
        ## define loss function
        if (input$loss == "absolute"){
            lossFn <- function(par) {
                sum(abs(y - par[1] - par[2] * x))
            }
        } else {
            lossFn <- function(par) {
                sum((y - par[1] - par[2] * x)^2)
            }
        }
        ## get y
        y <- switch(input$data,
                    "anscombe 1" = anscombe[["y1"]],
                    "anscombe 2" = anscombe[["y2"]],
                    "anscombe 3" = anscombe[["y3"]],
                    "anscombe 4" = anscombe[["y4"]],
                    "cars" = cars[["dist"]],
                    "cats" = cats[["Hwt"]],
                    "faithful" = faithful[["eruptions"]],
                    "thuesen" = thuesen[["short.velocity"]])
        ylab <- switch(input$data,
                       "cars" = "dist",
                       "cats" = "Hwt",
                       "faithful" = "eruptions",
                       "thuesen" = "short.velocity",
                       "y")
        ## get x
        x <- switch(input$data,
                    "anscombe 1" = anscombe[["x1"]],
                    "anscombe 2" = anscombe[["x2"]],
                    "anscombe 3" = anscombe[["x3"]],
                    "anscombe 4" = anscombe[["x4"]],
                    "cars" = cars[["speed"]],
                    "cats" = cats[["Bwt"]],
                    "faithful" = faithful[["waiting"]],
                    "thuesen" = thuesen[["blood.glucose"]])
        xlab <- switch(input$data,
                       "cars" = "speed",
                       "cats" = "Bwt",
                       "thuesen" = "blood.glucose",
                       "faithful" = "waiting",
                       "x")
        na <- is.na(y) | is.na(x)
        y <- y[!na]
        x <- x[!na]
        par <- optim(c(0.1, 0.1), lossFn, "BFGS")$par
        a <- round(par[[1]], 3)
        b <- round(par[[2]], 3)
        ## range +/- 20%
        aStep <- abs(a) * 0.02 + 5e-3
        aRange <- c(a - 10*aStep, a + 10*aStep)
        bStep <- abs(b) * 0.02 + 5e-3
        bRange <- c(b - 10*bStep, b + 10*bStep)
        ## start intercept and slope at fitted value
        output$intercept <- renderUI({
            numericInput("a",
                         "Intercept (a)",
                         value = a,
                         min = aRange[1],
                         max = aRange[2],
                         step = aStep)
        })
        output$slope <- renderUI({
            numericInput("b",
                         "Slope (b)",
                         value = b,
                         min = bRange[1],
                         max = bRange[2],
                         step = bStep)
        })
        ## plot data and interactive line
        par(mfrow = c(1, 2)) # layout won't work with points etc
        plot(y ~ x, ylab = ylab, xlab = xlab)
        abline(input$a, input$b, col = inputCol)
        mtext(bquote(E(.(ylab)) == .(input$a) + .(input$b) %.% .(xlab)),
              side = 3, line = 3, col = inputCol)
        ## plus regression line if requested
        if (input$fitted) {
            abline(a, b, col = abCol)
            mtext(bquote(E(.(ylab)) == .(a) + .(b) %.% .(xlab)),
                  side = 3, line = 1, col = abCol)
        }
        ## plus residuals if requested
        if (input$residuals) {
            yhat <- input$a + input$b * x
            res <- y - yhat
            segments(x0 = x, y0 = y, x1 = x, y1 = yhat,
                     col = resCol[sign(res)*0.5 + 1.5])
        }
        ## plus squared residuals if requested
        if (input$squares) {
            yhat <- input$a + input$b * x
            res <- y - yhat
            segments(x0 = c(x, x, x - res, x - res),
                     y0 = c(y, yhat, yhat, y),
                     x1 = c(x, x - res, x - res, x),
                     y1 = c(yhat, yhat, y, y),
                     col = resCol[sign(res)*0.5 + 1.5])
        }
        ## use log color scale to "zoom" into minimum
        ## (means that limits less critical)
        plotLoss <- function(type, loss, a, b){
            gr <- expand.grid(a = a, b = b)
            l <- apply(gr, 1, loss)
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
            image.plot(a, b, nLog2(l, n),
                       col = rev(dichromat_pal("BrowntoBlue.12")(12)),
                       axis.args = list(at = nLog2(ticks, n), labels=ticks),
                       legend.width = 1, smallplot= c(.80,.85,0.25,0.8))
        }
        plotLoss(input$loss, lossFn,
                 seq(aRange[1] - aStep, aRange[2] + aStep, length = 100),
                 seq(bRange[1] - bStep, bRange[2] + bStep, length = 100))
        points(input$a, input$b, col = inputCol, pch = 16, cex = 1.5)
        if (input$loss == "absolute") {
            lossEqn <-  eval(parse(text = paste0("quote(sum(group(\"|\", ",
                                       ylab, "[i] - a - b %.% ", xlab,
                                       "[i], \"|\"), i, NULL))")))
        } else {
            lossEqn <- eval(parse(text = paste0("quote(sum((", ylab,
                                      "[i] - a - b %.% ", xlab,
                                      "[i])^2, i, NULL))")))
        }
        mtext(bquote(paste(.(lossEqn), " = ", .(lossFn(c(input$a, input$b))))),
              side = 3, line = 2, col = inputCol)
        if (input$fitted) {
            points(a, b, col = abCol, pch = 16, cex = 1.5)
            mtext(bquote(paste(.(lossEqn), " = ", .(lossFn(c(a, b))))),
                  side = 3, line = 0, col = abCol)
        }
    })
})
