#include <omp.h>
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::plugins(openmp)]]
// [[Rcpp::export]]
NumericMatrix
Cbiweight_radius(
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

  NumericMatrix grid_val(nb_grid_pts, nb_var);

  #pragma omp parallel for schedule(static)
  for (int i = 0; i < nb_input_pts; ++i) {
    double d2;
    double pond;
    double sumpond = 0;
    double iradius = radius[i];
    double iradius2 = iradius * iradius;
    std::vector< std::tuple<int, double> > ponds;
    ponds.reserve(1024);

    for (int j = 0; j < nb_grid_pts; ++j) {
      double x = input_x[i] - grid_x[j];
      double y = input_y[i] - grid_y[j];

      if (x > iradius)
        continue;
      if (y > iradius)
        continue;
      if (x < -iradius)
        continue;
      if (y < -iradius)
        continue;

      d2 = x * x + y * y;

      if (d2 < iradius2) {
        pond = 1 - d2 / iradius2;
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
      for (auto [ j, jpond ] : ponds) {
        for (int k = 0; k < nb_var; ++k) {
          #pragma omp atomic update
          grid_val(j, k) += jpond * input_val(i, k) / sumpond;
        }
      }
    }
  }
  return grid_val;
}

// [[Rcpp::export]]
NumericMatrix
Cbiweight(
  NumericVector grid_x,
  NumericVector grid_y,
  NumericVector input_x,
  NumericVector input_y,
  NumericMatrix input_val,
  double radius,
  bool normalize)
{
  int nb_input_pts = input_x.size();
  int nb_grid_pts = grid_x.size();
  int nb_var = input_val.ncol();

  NumericMatrix grid_val(nb_grid_pts, nb_var);

  double radius2 = radius * radius;

  #pragma omp parallel for schedule(static)
  for (int i = 0; i < nb_input_pts; ++i) {
    double d2;
    double pond;
    double sumpond = 0;
    std::vector< std::tuple<int, double> > ponds;
    ponds.reserve(1024);

    for (int j = 0; j < nb_grid_pts; ++j) {
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
      for (auto [ j, jpond ] : ponds) {
        for (int k = 0; k < nb_var; ++k) {
          #pragma omp atomic update
          grid_val(j, k) += jpond * input_val(i, k) / sumpond;
        }
      }
    }
  }
  return grid_val;
}
