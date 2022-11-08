// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include "lazyNumbers_types.h"
#include <RcppEigen.h>
#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// nv2lvx
lazyVectorXPtr nv2lvx(Rcpp::NumericVector nv);
RcppExport SEXP _lazyNumbers_nv2lvx(SEXP nvSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type nv(nvSEXP);
    rcpp_result_gen = Rcpp::wrap(nv2lvx(nv));
    return rcpp_result_gen;
END_RCPP
}
// lvx2nv
Rcpp::NumericVector lvx2nv(lazyVectorXPtr lvx);
RcppExport SEXP _lazyNumbers_lvx2nv(SEXP lvxSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< lazyVectorXPtr >::type lvx(lvxSEXP);
    rcpp_result_gen = Rcpp::wrap(lvx2nv(lvx));
    return rcpp_result_gen;
END_RCPP
}
// minus_lvx
lazyVectorXPtr minus_lvx(lazyVectorXPtr lvx);
RcppExport SEXP _lazyNumbers_minus_lvx(SEXP lvxSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< lazyVectorXPtr >::type lvx(lvxSEXP);
    rcpp_result_gen = Rcpp::wrap(minus_lvx(lvx));
    return rcpp_result_gen;
END_RCPP
}
// lvx_plus_lvx
lazyVectorXPtr lvx_plus_lvx(lazyVectorXPtr lvx1, lazyVectorXPtr lvx2);
RcppExport SEXP _lazyNumbers_lvx_plus_lvx(SEXP lvx1SEXP, SEXP lvx2SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< lazyVectorXPtr >::type lvx1(lvx1SEXP);
    Rcpp::traits::input_parameter< lazyVectorXPtr >::type lvx2(lvx2SEXP);
    rcpp_result_gen = Rcpp::wrap(lvx_plus_lvx(lvx1, lvx2));
    return rcpp_result_gen;
END_RCPP
}
// lvx_minus_lvx
lazyVectorXPtr lvx_minus_lvx(lazyVectorXPtr lvx1, lazyVectorXPtr lvx2);
RcppExport SEXP _lazyNumbers_lvx_minus_lvx(SEXP lvx1SEXP, SEXP lvx2SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< lazyVectorXPtr >::type lvx1(lvx1SEXP);
    Rcpp::traits::input_parameter< lazyVectorXPtr >::type lvx2(lvx2SEXP);
    rcpp_result_gen = Rcpp::wrap(lvx_minus_lvx(lvx1, lvx2));
    return rcpp_result_gen;
END_RCPP
}
// lvx_times_lvx
lazyVectorXPtr lvx_times_lvx(lazyVectorXPtr lvx1, lazyVectorXPtr lvx2);
RcppExport SEXP _lazyNumbers_lvx_times_lvx(SEXP lvx1SEXP, SEXP lvx2SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< lazyVectorXPtr >::type lvx1(lvx1SEXP);
    Rcpp::traits::input_parameter< lazyVectorXPtr >::type lvx2(lvx2SEXP);
    rcpp_result_gen = Rcpp::wrap(lvx_times_lvx(lvx1, lvx2));
    return rcpp_result_gen;
END_RCPP
}
// lvx_dividedby_lvx
lazyVectorXPtr lvx_dividedby_lvx(lazyVectorXPtr lvx1, lazyVectorXPtr lvx2);
RcppExport SEXP _lazyNumbers_lvx_dividedby_lvx(SEXP lvx1SEXP, SEXP lvx2SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< lazyVectorXPtr >::type lvx1(lvx1SEXP);
    Rcpp::traits::input_parameter< lazyVectorXPtr >::type lvx2(lvx2SEXP);
    rcpp_result_gen = Rcpp::wrap(lvx_dividedby_lvx(lvx1, lvx2));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_lazyNumbers_nv2lvx", (DL_FUNC) &_lazyNumbers_nv2lvx, 1},
    {"_lazyNumbers_lvx2nv", (DL_FUNC) &_lazyNumbers_lvx2nv, 1},
    {"_lazyNumbers_minus_lvx", (DL_FUNC) &_lazyNumbers_minus_lvx, 1},
    {"_lazyNumbers_lvx_plus_lvx", (DL_FUNC) &_lazyNumbers_lvx_plus_lvx, 2},
    {"_lazyNumbers_lvx_minus_lvx", (DL_FUNC) &_lazyNumbers_lvx_minus_lvx, 2},
    {"_lazyNumbers_lvx_times_lvx", (DL_FUNC) &_lazyNumbers_lvx_times_lvx, 2},
    {"_lazyNumbers_lvx_dividedby_lvx", (DL_FUNC) &_lazyNumbers_lvx_dividedby_lvx, 2},
    {NULL, NULL, 0}
};

RcppExport void R_init_lazyNumbers(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
