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
biweight <- function(value, grid, radius = NULL, normalize = TRUE, var = NULL, id = NULL)
{
  stopifnot(is(value, "sf"), is(grid, "sf"))
  stopifnot(is.character(radius) || is.numeric(radius))

  if (is.character(radius)) {
    p_radius = value$radius
    # remove 'radius' var from value vars
    id = c(id, radius)
  } else {
    p_radius = radius
  }
  # if var is not specfified then keep all numeric value
  if (is.null(var)) {
    var = names(value)[sapply(value, is.numeric)]
  }
  # remove id from var
  if (!is.null(id)) {
    var = var[!var %in% id]
  }

  values = sf::st_drop_geometry(value[, var])
  biw = biweight_num(
    sf::st_coordinates(grid),
    sf::st_coordinates(value),
    as.matrix(values),
    radius = p_radius,
    normalize = normalize
  )
  ret = cbind(biw, grid)
  names(ret) = c(var, names(grid))
  ret
}

#' Biweight raw function
#'
#' @param grid      2 cols x,y numeric matrix of grid coordinates
#' @param input     2 cols x,y numeric matrix of value coordinates
#' @param value     matrix of values
#' @param radius    numeric vector of radius (length 1 or same as x and y)
#' @param normalize if TRUE, ensure sum of pond = 1 else no correction
#' @export
biweight_num <- function(grid, input, value, radius, normalize = TRUE)
{
  if (length(radius) != 1 && length(radius) != nrow(input))
    stop("`radius` must be a single value or have the same length as `input`")
  if (ncol(grid) != 2)
    stop("'grid' must have 2 columns x and y")
  if (ncol(input) != 2)
    stop("'input' must have 2 columns x and y")
  if (nrow(input) != nrow(value))
    stop("'input' and 'value' must have the same number of rows")

  Cbiweight(grid, input, value, radius, normalize)
}
