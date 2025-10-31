#include <Rcpp.h>
#include <omp.h>
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
  NumericVector radius,
  bool normalize)
{
  int nb_input_pts = input_x.size();
  int nb_grid_pts = grid_x.size();
  int nb_var = input_val.ncol();
  bool constant_radius = radius.size() == 1;

  NumericMatrix grid_val(nb_grid_pts, nb_var);

  #pragma omp parallel for
  for (int i = 0; i < nb_input_pts; ++i) {
    double sumpond = 0;
    double t_radius = constant_radius ? radius[0] : radius[i];
    std::vector< std::tuple<int, double> > ponds;
    ponds.reserve(1024);

    for (int j = 0; j < nb_grid_pts; ++j) {
      double x = input_x[i] - grid_x[j];
      double y = input_y[i] - grid_y[j];

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
          for (int k = 0; k < nb_var; ++k) {
            #pragma omp atomic update
            grid_val(j, k) += pond * input_val(i, k);
          }
        }
      }
    }
    if (normalize && sumpond > 0) {
      for (auto [ j, pond ] : ponds) {
        for (int k = 0; k < nb_var; ++k) {
          #pragma omp atomic update
          grid_val(j, k) += pond * input_val(i, k) / sumpond;
        }
      }
    }
  }
  return grid_val;
}
