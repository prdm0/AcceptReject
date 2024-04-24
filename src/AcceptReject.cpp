// #include <RcppArmadillo.h>
//
// // [[Rcpp::depends(RcppArmadillo)]]
// // [[Rcpp::plugins(cpp11)]]
//
// using namespace Rcpp;
//
// // [[Rcpp::export]]
//  arma::vec one_step(int n, Function f, Function f_base, Function random_base, double c) {
//    arma::vec x;
//    while (x.size() < n) {
//      arma::vec x_cand = as<arma::vec>(random_base(n));
//      arma::vec u = as<arma::vec>(runif(n));
//      arma::uvec accepted = find(u <= as<arma::vec>(f(wrap(x_cand))) / (c * as<arma::vec>(f_base(wrap(x_cand)))));
//      x = join_cols(x, x_cand.elem(accepted));
//    }
//    return x.rows(0, n-1);
//  }

#include <RcppArmadillo.h>

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(cpp11)]]

using namespace Rcpp;

// [[Rcpp::export]]
arma::vec one_step(int n, Function f, Function f_base, Function random_base, double c) {
  arma::vec x(n);
  int filled = 0;

  while (filled < n) {
    arma::vec x_cand = as<arma::vec>(random_base(n));
    arma::vec fx_cand = as<arma::vec>(f(wrap(x_cand)));
    arma::vec u = as<arma::vec>(runif(n));
    arma::uvec accepted = find(u <= fx_cand / (c * as<arma::vec>(f_base(wrap(x_cand)))));

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
