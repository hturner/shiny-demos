## get plot limits for given variable as for regular axes in R
## i.e. par("xaxs") or par("yaxs") equal to "r"
getLim <- function(x) {
     xlim <- range(x)
     xlim + c(-0.04, 0.04) * diff(xlim)
 }

