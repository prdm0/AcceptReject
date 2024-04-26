#include <RcppArmadillo.h>

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(cpp11)]]

using namespace Rcpp;

// [[Rcpp::export]]
arma::vec one_step(unsigned long int n, Function f, Function f_base, Function random_base, double c) {
  arma::vec x(n);
  arma::vec x_cand(n);
  arma::vec fx_cand(n);
  arma::vec u(n);
  arma::uvec accepted(n);
  unsigned long int filled = 0;

  while (filled < n) {
    x_cand = as<arma::vec>(random_base(n));
    fx_cand = as<arma::vec>(f(wrap(x_cand)));
    u = as<arma::vec>(runif(n));
    accepted = find(u <= fx_cand / (c * as<arma::vec>(f_base(wrap(x_cand)))));

    int num_accepted = accepted.n_elem;
    if (num_accepted > 0) {
      int space_left = n - filled;
      if (num_accepted > space_left) {
        num_accepted = space_left;
        accepted = accepted.subvec(0, space_left - 1);
      }
      x.subvec(filled, filled + num_accepted - 1) = x_cand.elem(accepted);
      filled += num_accepted;
    }
  }
  return x;
}
