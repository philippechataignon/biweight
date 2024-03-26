#include <omp.h>
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::plugins(openmp)]]
// [[Rcpp::export]]

NumericMatrix
Cbiweight(
  NumericVector grid_x,
  NumericVector grid_y,
  NumericVector input_x,
  NumericVector input_y,
  NumericMatrix input_val,
  double radius,
  bool ind_normalize)
{
  int nr = grid_x.size();
  int nc = input_val.ncol();

  NumericMatrix hexval(nr, nc);

  double radius2 = radius * radius;

  #pragma omp parallel for schedule(static)
  for (int i = 0; i < input_x.size(); ++i) {
    double d2;
    double pond;
    double sumpond = 0;
    std::vector< std::tuple<int, double> > ponds;
    ponds.reserve(1024);

    for (int j = 0; j < nr; ++j) {
      double x = input_x[i] - grid_x[j];
      double y = input_y[i] - grid_y[j];

      if (x > radius)
        continue;
      if (y > radius)
        continue;
      if (x < -radius)
        continue;
      if (y < -radius)
        continue;

      d2 = x * x + y * y;

      if (d2 < radius2) {
        pond = 1 - d2 / radius2;
        pond *= pond;
        if (ind_normalize) {
          ponds.push_back(std::make_tuple(j, pond));
          sumpond += pond;
        } else {
          for (int k = 0; k < nc; ++k) {
            #pragma omp atomic update
            hexval(j, k) += pond * input_val(i, k);
          }
        }
      }
    }
    if (ind_normalize && sumpond > 0) {
      for (auto [ j, jpond ] : ponds) {
        for (int k = 0; k < nc; ++k) {
          #pragma omp atomic update
          hexval(j, k) += jpond * input_val(i, k) / sumpond;
        }
      }
    }
  }
  return hexval;
}
