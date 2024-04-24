// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// one_step
arma::vec one_step(unsigned long int n, Function f, Function f_base, Function random_base, double c);
RcppExport SEXP _AcceptReject_one_step(SEXP nSEXP, SEXP fSEXP, SEXP f_baseSEXP, SEXP random_baseSEXP, SEXP cSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< unsigned long int >::type n(nSEXP);
    Rcpp::traits::input_parameter< Function >::type f(fSEXP);
    Rcpp::traits::input_parameter< Function >::type f_base(f_baseSEXP);
    Rcpp::traits::input_parameter< Function >::type random_base(random_baseSEXP);
    Rcpp::traits::input_parameter< double >::type c(cSEXP);
    rcpp_result_gen = Rcpp::wrap(one_step(n, f, f_base, random_base, c));
    return rcpp_result_gen;
END_RCPP
}
// best_y
double best_y(NumericVector xlim, Function f, Function f_base, bool continuous, double epsilon);
RcppExport SEXP _AcceptReject_best_y(SEXP xlimSEXP, SEXP fSEXP, SEXP f_baseSEXP, SEXP continuousSEXP, SEXP epsilonSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type xlim(xlimSEXP);
    Rcpp::traits::input_parameter< Function >::type f(fSEXP);
    Rcpp::traits::input_parameter< Function >::type f_base(f_baseSEXP);
    Rcpp::traits::input_parameter< bool >::type continuous(continuousSEXP);
    Rcpp::traits::input_parameter< double >::type epsilon(epsilonSEXP);
    rcpp_result_gen = Rcpp::wrap(best_y(xlim, f, f_base, continuous, epsilon));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_AcceptReject_one_step", (DL_FUNC) &_AcceptReject_one_step, 5},
    {"_AcceptReject_best_y", (DL_FUNC) &_AcceptReject_best_y, 5},
    {NULL, NULL, 0}
};

RcppExport void R_init_AcceptReject(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
