#' Biweight function
#'
#' @param value : sf values + points
#' @param grid : sf points
#' @param id : names of id variable in grid
#' @param radius : 20000 for 20 km
#' @param ind_normalize : if TRUE, ensure sum of pond = 1
#' @export
biweight <- function(value, grid, radius, ind_normalize, id = NULL)
{
  stopifnot("sf" %in% class(value), "sf" %in% class(grid))
  varnum = names(value)[sapply(value, is.numeric)]
  if (!is.null(id)) {
    varnum = varnum[varnum != id]
  }
  values = sf::st_drop_geometry(value[, varnum])
  biw = Cbiweight(
    sf::st_coordinates(grid)[,1],
    sf::st_coordinates(grid)[,2],
    sf::st_coordinates(value)[,1],
    sf::st_coordinates(value)[,2],
    as.matrix(values),
    radius = radius,
    ind_normalize = as.integer(ind_normalize)
  )
  ret = cbind(biw, grid)
  names(ret) = c(varnum, names(grid))
  ret
}
