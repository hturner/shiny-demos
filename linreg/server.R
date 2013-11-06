library(colorspace)
library(fields)
## server script for linear regression app
shinyServer(function(input, output, session){
    output$distPlot <- renderPlot({
        ## get data and regression line
        if (input$loss == "absolute"){
            lossFn <- function(par) {
                sum(abs(y - par[1] - par[2] * x))
            }
        } else { ## use sqrt so on same scale
            lossFn <- function(par) {
                sum((y - par[1] - par[2] * x)^2)
            }
        }
        x <- c(1,2,2,3,3,4,4,5,5,6)
        y <- c(2,1,3,2,4,3,5,4,6,5)
        par <- optim(c(0, 0), lossFn, "BFGS")$par
        a <- round(par[[1]], 3)
        b <- round(par[[2]], 3)
        aRange <- c(a - 0.6, a + 0.6)
        aStep <- 0.1
        bRange <- c(b - 0.6, b + 0.6)
        bStep <- 0.1
        ## start intercept and slope at fitted value
        #intercept.initial <- runif(1, 0.7 * min(y) + 0.3 * max(y),
        #    0.3 * min(y) + 0.7 * max(y))
        #intercept.delta <- diff(range(y))/200
        #slope.delta <- (diff(range(y))/diff(range(x)))/50
        output$intercept <- renderUI({
            numericInput("a",
                         "intercept",
                         value = a,
                         min = aRange[1],
                         max = aRange[2],
                         step = aStep)
        })
        output$slope <- renderUI({
            numericInput("b",
                         "slope",
                         value = b,
                         min = bRange[1],
                         max = bRange[2],
                         step =bStep)
        })
        ## plot data and interactive line
        par(mfrow = c(1, 2))
        plot(y ~ x)
        abline(input$a, input$b, col = "blue")
        mtext(paste("E(y) =", input$a, "+", input$b, "* x"),
              side = 3, line = 3, col = "blue")
        ## plus regression line if requested
        if (input$fitted) {
            abline(a, b, col = "green")
            mtext(paste("E(y) =", a, "+", b, "* x"),
                  side = 3, line = 1, col = "green")
        }
        ## plus residuals if requested
        if (input$residuals) {
            segments(x0 = x, y0 = y, x1 = x, y1 = input$a + input$b * x,
                     col = "red")
        }
        ## use log color scale to "zoom" into minimum
        ## (means that limits less critical)
        plotLoss <- function(loss, a, b){
            gr <- expand.grid(a = a, b = b)
            l <- apply(gr, 1, loss)
            l <- matrix(l, length(a), length(b))
            tripleLog <- function(x) log2(log2(log2(x)))
            ticks <- c(2.5, 5, 10, 20, 40, 80, 160)
            #ticks <- seq(5, 200, by = 5)
            image.plot(a, b, tripleLog(l), col = rev(heat.colors(12)),
                       axis.args = list(at = tripleLog(ticks), labels=ticks))
        }
        plotLoss(lossFn,
                 seq(aRange[1] - aStep, aRange[2] + aStep, length = 100),
                 seq(bRange[1] - aStep, bRange[2] + bStep, length = 100))
        points(input$a, input$b, col = "blue")
        if (input$loss == "absolute") {
            lossEqn <- quote(sum(group("|", y[i] - a - bx[i], "|"), i, NULL))
        } else {
            lossEqn <- quote(sum((y[i] - a - bx[i])^2, i, NULL))
        }
        mtext(bquote(paste(.(lossEqn), " = ", .(lossFn(c(input$a, input$b))))),
              side = 3, line = 2, col = "blue")
        if (input$fitted) {
            points(a, b, col = "green")
            mtext(bquote(paste(.(lossEqn), " = ", .(lossFn(c(a, b))))),
                  side = 3, line = 0, col = "green")
        }
    })
})
