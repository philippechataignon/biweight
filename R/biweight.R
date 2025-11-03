#' Biweight function
#'
#' @param value     sf table of input values with a geometry column 'point'
#' @param grid      sf table of grid points
#' @param radius    if character, name of the radius column in value
#' of row in value else numeric constant
#' @param normalize if TRUE, ensure sum of pond = 1 else no correction
#' @param var       names of 'value' variables in grid, by default all numeric
#' except 'id' variables
#' @param id        excluded variables from 'var'
#' @export
biweight <- function(value, grid, radius = NULL, normalize = TRUE, var = NULL)
{
  stopifnot(is(value, "sf"), is(grid, "sf"))
  stopifnot(is.character(radius) || is.numeric(radius))

  # if var is not specfified then var = all numeric value
  if (is.null(var)) {
    var = names(value)[sapply(value, is.numeric)]
  }
  if (is.character(radius)) {
    p_radius = value$radius
    # remove 'radius' var from value vars
    var = var[!var == radius]
  } else {
    p_radius = radius
  }
  biw = biweight_num(
    sf::st_coordinates(value),
    sf::st_coordinates(grid),
    as.matrix(sf::st_drop_geometry(value[, var])),
    radius = p_radius,
    normalize = normalize
  )
  ret = cbind(grid, biw)
  ret
}

#' Biweight raw function
#'
#' @param input     2 cols x,y numeric matrix of value coordinates
#' @param grid      2 cols x,y numeric matrix of grid coordinates
#' @param value     matrix of values
#' @param radius    numeric vector of radius (length 1 or same as x and y)
#' @param normalize if TRUE, ensure sum of pond = 1 else no correction
#' @export
biweight_num <- function(input, grid, value, radius, normalize = TRUE)
{
  if (length(radius) != 1 && length(radius) != nrow(input))
    stop("`radius` must be a single value or have the same length as `input`")
  if (ncol(grid) != 2)
    stop("'grid' must have 2 columns x and y")
  if (ncol(input) != 2)
    stop("'input' must have 2 columns x and y")
  if (nrow(input) != nrow(value))
    stop("'input' and 'value' must have the same number of rows")
  ret = Cbiweight(grid, input, value, radius, normalize)
  colnames(ret) <- colnames(value)
  ret
}
