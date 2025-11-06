#include <Rcpp.h>
#ifdef _OPENMP
#include <omp.h>
#endif
using namespace Rcpp;

// [[Rcpp::plugins("cpp17")]]
// [[Rcpp::plugins(openmp)]]
// [[Rcpp::export]]
NumericMatrix
Cbiweight(
  NumericMatrix input,
  NumericMatrix grid,
  NumericMatrix value,
  NumericVector radius,
  bool normalize)
{
  bool constant_radius = radius.size() == 1;
  NumericMatrix grid_val(grid.nrow(), value.ncol());

  #pragma omp parallel for
  for (int i = 0; i < input.nrow(); ++i) {
    double sumpond = 0;
    double t_radius = constant_radius ? radius[0] : radius[i];
    std::vector< std::tuple<int, double> > ponds;
    ponds.reserve(1024);

    for (int j = 0; j < grid.nrow(); ++j) {
      double x = input(i, 0) - grid(j, 0);
      double y = input(i, 1) - grid(j, 1);

      if (x > t_radius)
        continue;
      if (y > t_radius)
        continue;
      if (x < -t_radius)
        continue;
      if (y < -t_radius)
        continue;

      double pond = 0;
      double d2 = x * x + y * y;
      double t_radius2 = t_radius * t_radius;

      if (d2 < t_radius2) {
        pond = 1 - d2 / t_radius2;
        pond *= pond;
        if (normalize) {
          ponds.push_back(std::make_tuple(j, pond));
          sumpond += pond;
        } else {
          for (int k = 0; k < value.ncol(); ++k) {
            #pragma omp atomic update
            grid_val(j, k) += pond * value(i, k);
          }
        }
      }
    }
    if (normalize && sumpond > 0) {
      for (auto [ j, pond ] : ponds) {
        for (int k = 0; k < value.ncol(); ++k) {
          #pragma omp atomic update
          grid_val(j, k) += pond * value(i, k) / sumpond;
        }
      }
    }
  }
  return grid_val;
}
