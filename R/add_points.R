#' Add points
#'
#' This function implements an algorithm that add a small habitat portion to a
#' given landscape.
#'
#' @param xy Coordinates of the points.
#' @param landscape Polygon of the focal landscape.
#' @param width Parameter for the IIC calculation.
#'
#' @returns A new polygon, similiar to the inputed, but with an small point
#' with additional habitat.
#'
#' @examples
#' pts = rbind(c(-45,23))
#' l = rgdal::readOGR('./path to shapefile directory/')
#' add_points(pts, l, 0.1)
#'
#' @export
add_points = function(xy, landscape, width){

  buff = gBuffer(xy, width=width)

  output = bind(landscape, buff)
  output = aggregate(output)

  return(output)

}
