#' @useDynLib biweight
#' @importFrom Rcpp evalCpp
.onAttach <- function(libname, pkgname) {
  if (!interactive()) return
  packageStartupMessage(paste("Biweight", utils::packageVersion("biweight")))
}
