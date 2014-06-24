## use log color scale to "zoom" into minimum
## (means that limits are less critical)
plotLoss <- function(type, loss, a, b, y, x){
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
               smallplot= c(.85,.9,0.2,0.8))
}
