#' Biweight function
#'
#' @param value : sf values + points
#' @param grid : sf points
#' @param id : names of id variable in grid
#' @param radius : 20000 for 20 km
#' @param normalize : if TRUE, ensure sum of pond = 1 else no correction
#' @param output_grid_geometry : is `geometry` present in return value ?
#' @export
biweight <- function(value, grid, radius, normalize = TRUE, output_grid_geometry = FALSE, var = NULL, id = NULL)
{
  stopifnot("sf" %in% class(value), "sf" %in% class(grid))
  if (is.null(var)) {
    var = names(value)[sapply(value, is.numeric)]
  }
  if (!is.null(id)) {
    var = var[var != id]
  }
  values = sf::st_drop_geometry(value[, var])
  grid_coord = sf::st_coordinates(grid)
  value_coord = sf::st_coordinates(value)
  if (is.atomic(radius))
  biw = biweight_num(
    grid_coord[,1],
    grid_coord[,2],
    value_coord[,1],
    value_coord[,2],
    as.matrix(values),
    radius = radius,
    normalize = normalize
  )
  if (!output_grid_geometry) {
    grid = sf::st_drop_geometry(grid)
  }
  ret = cbind(biw, grid)
  names(ret) = c(var, names(grid))
  ret
}

#' Biweight raw function
#'
#' @param grid.x = numeric vector of x grid coordinates
#' @param grid.y = numeric vector of y grid coordinates
#' @param value.x = numeric vector of x value coordinates
#' @param value.y = numeric vector of y value coordinates
#' @param value.matrix = matrix of values
#' @param radius : radius in same unit as x, y
#' @param normalize : if TRUE, ensure sum of pond = 1 else no correction
#' @export
biweight_num <- function(grid.x, grid.y, value.x, value.y, value.matrix, radius, normalize = TRUE)
{
  if (length(radius) != 1 && length(radius) != length(value.x))
    stop("`radius` must be a single value or have the same length as `value`")

  if (length(radius) == 1)
    fbiw = Cbiweight
  else
    fbiw = Cbiweight_radius
  fbiw(
    grid.x,
    grid.y,
    value.x,
    value.y,
    value.matrix,
    radius = radius,
    normalize = normalize
  )
}
