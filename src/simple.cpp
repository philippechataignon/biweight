#include <omp.h>
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::plugins(openmp)]]
// [[Rcpp::export]]


NumericMatrix
Csimple(NumericVector grid_x, NumericMatrix val)
{
  NumericMatrix sumpond(grid_x.size(), val.ncol());
    #pragma omp parallel for
    for (int i = 0; i < val.nrow(); ++i) {
      for (int j = 0; j < grid_x.size(); ++j) {
        for (int k = 0; k < val.ncol(); ++k) {
          sumpond(j, k) += val(i, k);
          sumpond(i, 0) = omp_get_thread_num();
        }
      }
    }
  return sumpond;
}
