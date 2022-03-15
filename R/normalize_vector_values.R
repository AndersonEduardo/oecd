#' MinMax normalization
#'
#' Receives a vector of numeric values and apply the min-max
#' normalization (or rescaling).
#'
#' @param x A numeric vector.
#'
#' @returns A numeric vector, with values rescaled between 0 and 1.
#'
#' @examples
#' v = 1:10
#' normalize_vector_values(v)
#'
#' @export
normalize_vector_values = function(x){

  return(

    (x - min(x, na.rm=TRUE)) / (max(x, na.rm=TRUE) - min(x, na.rm=TRUE))

  )

}
