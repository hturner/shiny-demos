## add box to base of 3D plot
base3D <- function(xlim, ylim, zlim, P){
    lines(trans3d(xlim[1], ylim, zlim[1], P))
    lines(trans3d(xlim[2], ylim, zlim[1], P))
    lines(trans3d(xlim, ylim[1], zlim[1], P))
    lines(trans3d(xlim, ylim[2], zlim[1], P))
}
