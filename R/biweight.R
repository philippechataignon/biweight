#' Biweight fuction
#'
#' @param value : sf values + points
#' @param grid : sf points
#' @param id : names of id variable in grid
#' @param radius : 20000 for 20 km
#' @param ind_normalize : if TRUE, ensure sum of pond = 1
#' @export
biweight <- function(value, grid, id, radius, ind_normalize)
{
  stopifnot("sf" %in% class(value), "sf" %in% class(grid))
  varnum = names(value)[sapply(value, is.numeric)]
  varnum = varnum[varnum != id]
  values = st_drop_geometry(value[, varnum])
  biw = Cbiweight(
    st_coordinates(grid)[,1],
    st_coordinates(grid)[,2],
    st_coordinates(value)[,1],
    st_coordinates(value)[,2],
    as.matrix(values),
    radius = radius,
    ind_normalize = as.integer(ind_normalize)
  )
  ret = cbind(st_drop_geometry(grid[, id]), biw)
  names(ret) = c(id, varnum)
  ret
}
