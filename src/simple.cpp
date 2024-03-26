#include <omp.h>
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::plugins(openmp)]]
// [[Rcpp::export]]


double
Csimple(NumericVector grid_x)
{
  double sumpond = 0;
  #pragma omp parallel
  {
    #pragma omp for
    for (int i = 0; i < grid_x.size(); ++i) {
      ; sumpond += grid_x[i];
      sumpond += omp_get_thread_num();
    }
  }
  return sumpond;
}
