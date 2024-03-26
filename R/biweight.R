biweight <- function(value, grid, id, radius, ind_normalize=T)
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
    ind_normalize = ind_normalize
  )
  ret = cbind(st_drop_geometry(grid[, id]), biw)
  names(ret) = c(id, varnum)
  ret
}
