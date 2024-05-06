// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(cpp11)]]

#include <RcppArmadillo.h>

using namespace Rcpp;

// [[Rcpp::export]]
arma::vec one_step(unsigned long int n, Function f, Function f_base, Function random_base, double c) {
  arma::vec x(n);
  arma::vec x_cand(n);
  arma::vec fx_cand(n);
  arma::vec fx_base(n);
  arma::vec u(n);
  unsigned long int filled = 0;

  while (filled < n) {
    x_cand = as<arma::vec>(random_base(n));
    fx_cand = as<arma::vec>(f(wrap(x_cand)));
    fx_base = as<arma::vec>(f_base(wrap(x_cand)));
    fx_base *= c;
    u = arma::randu<arma::vec>(n);
    arma::uvec accepted = find(u <= fx_cand / fx_base);

    unsigned long int num_accepted = accepted.n_elem;
    if (num_accepted > 0) {
      unsigned long int space_left = n - filled;
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

// [[Rcpp::export]]
double internal_quantile(bool continuous, Rcpp::Function f, double p, double xlim1, double xlim2) {
  arma::vec x, y;
  if (!continuous) {
    x = arma::regspace(xlim1, xlim2);
    y = Rcpp::as<arma::vec>(f(x));
    y = arma::cumsum(y) / arma::sum(y);
    arma::uvec idx = arma::find(y >= p);
    return x[idx(0)];
  } else {
    unsigned long int n = 1e4;
    x = arma::linspace<arma::vec>(xlim1, xlim2, n);
    y = Rcpp::as<arma::vec>(f(x));
    y = arma::cumsum(y) / arma::sum(y);

    Rcpp::Function approx("approx");
    Rcpp::List res = approx(y, x, Rcpp::Named("xout", p));

    return Rcpp::as<double>(res["y"]);
  }
}
