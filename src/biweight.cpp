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

  double d2;
  double pond;
  double radius2 = radius * radius;

#pragma omp parallel
{
  NumericMatrix my_hexval(nr, nc);
  #pragma omp for
  for (int i = 0; i < input_x.size(); ++i) {
    double sumpond = 0;
    std::vector<int> liste_j;
    std::vector<double> liste_pond;
    liste_j.reserve(65536);
    liste_pond.reserve(65536);

    for (int j = 0; j < nr; ++j) {
      if (grid_x[j] < input_x[i] - radius)
        continue;
      if (grid_x[j] > input_x[i] + radius)
        continue;
      if (grid_y[j] < input_y[i] - radius)
        continue;
      if (grid_y[j] > input_y[i] + radius)
        continue;

      d2 = (input_x[i] - grid_x[j]) * (input_x[i] - grid_x[j]) + (input_y[i] - grid_y[j]) * (input_y[i] - grid_y[j]);

      if (d2 < radius2) {
        pond = 1 - d2 / radius2;
        pond *= pond;
        if (ind_normalize) {
          liste_j.push_back(j);
          liste_pond.push_back(pond);
          sumpond += pond;
        } else {
          for (int k = 0; k < nc; ++k) {
            my_hexval(j, k) += pond * input_val(i, k);
          }
        }
      }
    }
    if (ind_normalize && sumpond > 0) {
      std::vector<int>::iterator lj;
      std::vector<double>::iterator lp;
      for (lj = liste_j.begin(), lp = liste_pond.begin();
           lj != liste_j.end(); ++lj, ++lp) {
        for (int k = 0; k < nc; ++k) {
          my_hexval(*lj, k) += (*lp) * input_val(i, k) / sumpond;
        }
      }
    }
  }
  #pragma omp critical
  for (int j = 0; j < nr; ++j)
    for (int k = 0; k < nc; ++k)
      hexval(j, k) += my_hexval(j, k);
}
  return hexval;
}
