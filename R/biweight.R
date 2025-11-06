#' Biweight function
#'
#' @param value     sf table of input values with a geometry column 'point'
#' @param grid      sf table of grid points
#' @param radius    If character, name of the radius column in value else numeric constant
#' @param normalize If TRUE, the defaut, ensure sum of pond = 1 else no correction
#' @param var       Names of 'value' variables in grid, by default all numeric variables
#' @export
biweight <- function(value, grid, radius = NULL, normalize = TRUE, var = NULL)
{
  stopifnot(inherits(value, "sf"), inherits(grid, "sf"))
  stopifnot(is.character(radius) || is.numeric(radius))

  # if var is not specfified then var = all numeric value
  if (is.null(var)) {
    var = names(value)[sapply(value, is.numeric)]
  }
  if (is.character(radius)) {
    # remove 'radius' var from value vars
    var = var[!var == radius]
    radius = value$radius
  }

  biw = biweight_num(
    sf::st_coordinates(value),
    sf::st_coordinates(grid),
    as.matrix(sf::st_drop_geometry(value[, var])),
    radius = radius,
    normalize = normalize
  )
  ret = cbind(grid, biw)
  ret
}

#' Biweight raw function
#'
#' @param input     2 cols x,y numeric matrix of value coordinates
#' @param grid      2 cols x,y numeric matrix of grid coordinates
#' @param value     Matrix of values
#' @param radius    Numeric vector of radius (length 1 or same as x and y)
#' @param normalize If TRUE, ensure sum of pond = 1 else no correction
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
  ret = Cbiweight(input, grid, value, radius, normalize)
  colnames(ret) <- colnames(value)
  ret
}
