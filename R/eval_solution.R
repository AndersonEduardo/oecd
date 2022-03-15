#' Evaluate solution
#'
#' Compute the IIC (Integral Index of Connectivity) for a given landscape.
#'
#' @param solution_polygon The polygon for the focal landscape.
#' @param folder_path Path to the project directory.
#' @param max_dist Parameter for the IIC calculation.
#' @param habitat Value in which habitat is assigned in the solution_polygon.
#' @param metric The desired metric (currently, only implemented for IIC).
#'
#' @returns The IIC metric for the imputed landscape polygon.
#'
#' @examples
#' l = rgdal::readOGR('./path to shapefile directory/')
#' eval_solution(l, './path to my project directory/', 100, 1, 'IIC')
#'
#' @export
eval_solution = function(solution_polygon, folder_path, max_dist, habitat, metric='IIC'){

  writeOGR(solution_polygon, folder_path, 'solution_polygon',
           driver='ESRI Shapefile', overwrite_layer=TRUE, delete_dsn=TRUE)

  landscape = upload_land(file.path(folder_path, 'solution_polygon.shp'),
                          bound_path = NULL,
                          habitat = habitat,
                          max_dist = max_dist)

  metric = con_metric(landscape, metric=metric)

  return(metric)

}
