#' Matrix mapper
#'
#' Function that implements an algorithm for maping IIC value at a given (small)
#' point in the landscape, if it were restored.
#'
#' @param xy Coordinates of the points (to simulate habitat restoration).
#' @param landscape Polygon of the focal landscape.
#' @param folder_path Path to the project directory.
#' @param moving_window If use or not a moving window over the landscape polygon.
#' It is very usefull for performance with large data. Default is TRUE.
#' @param window_size Size for moving window. Only used if moving_window=TRUE.
#' @param restoration_size size of the point in which habitat restoration will be
#' simulated.
#' @param max_dist Parameter for the IIC calculation.
#' @param habitat Value in which habitat is assigned in the solution_polygon.
#' @param matrix Value in which matrix is assigned in the solution_polygon.
#' @param plot If plot or not plot the moving window through the function
#' iterations. It is usefull for debugging, but consumes computational time and
#' memmory.
#'
#' @returns A new polygon, similiar to the inputed, but with an small point
#' with additional habitat.
#'
#' @examples
#' pts = rbind(c(-45,23))
#' l = rgdal::readOGR('./path to shapefile directory/')
#' add_points(pts, l, 0.1)

#' @export
matrix_mapper = function(xy, landscape, folder_path, moving_window=TRUE,
                         window_size, restoration_size, max_dist, habitat=1,
                         matrix=0,  plot=FALSE){

  cat('\n[STATUS] Inicializando... ')
  total_time_start = Sys.time()
  # metric = NA
  xy_df = as.data.frame(xy)
  xy_df$metric = NA
  cat('pronto.\n')

  for (i in 1:length(xy)){

    point_time_start = Sys.time()

    if (moving_window == TRUE){

      cat('[STATUS] Computando geometria da janela deslizante... ')
      start = Sys.time()
      window_geometry = rgeos::gBuffer(xy[i], width=window_size)
      end = Sys.time()
      cat('pronto. (latência:', as.numeric(difftime(end, start, unit='secs')),
          'segundos)\n')

      if (plot == TRUE){
        cat('[STATUS] Executando plot... ')
        start = Sys.time()
        plot(landscape)
        plot(buffer_geometry, add=TRUE)
        plot(xy[i], add=TRUE)
        end = Sys.time()
        cat('pronto. (latência:', as.numeric(difftime(end, start, unit='secs')),
            'segundos)\n')
      }

      cat('[STATUS] Recortando paisagem para a geometria da janela... ')
      start = Sys.time()
      croped_area = raster::crop(
        landscape[landscape$layer==habitat, ],
        window_geometry
      )
      end = Sys.time()
      cat('pronto. (latência:', as.numeric(difftime(end, start, unit='secs')),
          'segundos)\n')

      if (is.null(croped_area)){

        next

      }

      cat('[STATUS] Ajustando área recortada... ')
      start = Sys.time()
      croped_area = sp::disaggregate(croped_area)
      end = Sys.time()
      cat('pronto. (latência:', as.numeric(difftime(end, start, unit='secs')),
          'segundos)\n')

    }else{

      croped_area = landscape[landscape$layer==habitat, ]

      cat('[STATUS] Executando plot... ')
      start = Sys.time()
      if (plot == TRUE){
        plot(croped_area)
        plot(xy[i], add=TRUE)
      }
      end = Sys.time()
      cat('pronto. (latência:', as.numeric(difftime(end, start, unit='secs')),
          'segundos)\n')

    }

    cat('[STATUS] Simulando implantação de área de habitat... ')
    start = Sys.time()
    focal_area = add_points(xy[i], croped_area, width=restoration_size)
    end = Sys.time()
    cat('pronto. (latência:', as.numeric(difftime(end, start, unit='secs')),
        'segundos)\n')

    cat('[STATUS] Realizando alguns ajustes... ')
    start = Sys.time()
    focal_area = sp::disaggregate(focal_area)

    focal_area = as(focal_area, "SpatialPolygonsDataFrame")
    names(focal_area@data) = 'layer'
    focal_area@data$layer = habitat
    end = Sys.time()
    cat('pronto. (latência:', as.numeric(difftime(end, start, unit='secs')),
        'segundos)\n')

    cat('[STATUS] Computando métrica... ')
    start = Sys.time()
    metric = tryCatch(
      eval_solution(
        solution_polygon = focal_area,
        folder_path = folder_path,
        max_dist = max_dist,
        habitat = habitat
      ), error=function(e){NA}
    )
    end = Sys.time()
    cat('pronto. (latência:', as.numeric(difftime(end, start, unit='secs')),
        'segundos)\n')

    cat('[STATUS] Registrando métrica computada...')
    xy_df[i,'metric'] = metric
    cat('pronto.\n')

    write.csv(xy_df, file=file.path(folder_path,'matrixmapper.csv'))

    cat('[STATUS] Finalizado ponto', i, ': metrica', metric, '\n')
    point_time_end = Sys.time()
    cat('[STATUS] Tempo da iteração:',
        difftime(point_time_end, point_time_start, unit='secs'),
        'segundos \n\n')
  }

  cat('[STATUS] Execução finalizada.')
  total_time_end = Sys.time()
  cat('[STATUS] Tempo total:',
      as.numeric(difftime(total_time_end, total_time_start, unit='secs')),
      'segundos.\n')

  return(xy_df)

}
