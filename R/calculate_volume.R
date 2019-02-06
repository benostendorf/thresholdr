##' Calculate volume of an ellipsoid
##'
##' This functions calculates the volume of an object given width and length
##' assuming an ellipsoid shape with equal width and height.
##' @param width numeric; width of object
##' @param length numeric; length of object
##' @return The rounded object volume
##' @export
calculate_volume <- function(width, length) {
  volume <- pi/6 * (width ^ 2) * length
  volume <- round(volume, 0)
}
