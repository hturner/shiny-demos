absoluteLoss <- function(par, y, x) {
    sum(abs(y - par[1] - par[2] * x))
}
quadraticLoss <- function(par, y, x) {
    sum((y - par[1] - par[2] * x)^2)
}
