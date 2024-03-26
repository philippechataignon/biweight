#' @useDynLib biweight
#' @importFrom Rcpp evalCpp
.onAttach <- function(libname, pkgname) {
  if (!interactive()) return
  packageStartupMessage(paste("Package biweight", utils::packageVersion("biweight")))
}
