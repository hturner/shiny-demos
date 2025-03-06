##' Plot the loss function for a regression model with a single explanatory
##' variable.
##'
##' The loss function is evaluated over a grid of values for the intercept
##' and the slope. A log colour scale is used to "zoom" into minimum.
##' @title Plot Loss Function
##' @param loss a function that returns the loss, with arguments \code{par}, a vector of intercept and slope, \code{y}, the reponse vector, and \code{x} the explanatory variable.
##' @param a sequence of values for intercept.
##' @param b sequence of values for slope.
##' @param y response vector.
##' @param x explanatory variable.
##' @author Heather Turner
##' @export
##' @importFrom fields image.plot
##' @importFrom scales dichromat_pal
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
    image.plot(a, b, nLog2(l, n),
               col = rev(dichromat_pal("BrowntoBlue.12")(12)),
               axis.args = list(at = nLog2(ticks, n), labels=ticks),
               smallplot= c(.84,.89,0.2,0.8))
}
