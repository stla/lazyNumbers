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

// lazyNA
lazyVectorXPtr lazyNA();
RcppExport SEXP _lazyNumbers_lazyNA() {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    rcpp_result_gen = Rcpp::wrap(lazyNA());
    return rcpp_result_gen;
END_RCPP
}
// lazyNAomit
lazyVectorXPtr lazyNAomit(lazyVectorXPtr lvx);
RcppExport SEXP _lazyNumbers_lazyNAomit(SEXP lvxSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< lazyVectorXPtr >::type lvx(lvxSEXP);
    rcpp_result_gen = Rcpp::wrap(lazyNAomit(lvx));
    return rcpp_result_gen;
END_RCPP
}
// isLazyNA
Rcpp::LogicalVector isLazyNA(lazyVectorXPtr lvx);
RcppExport SEXP _lazyNumbers_isLazyNA(SEXP lvxSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< lazyVectorXPtr >::type lvx(lvxSEXP);
    rcpp_result_gen = Rcpp::wrap(isLazyNA(lvx));
    return rcpp_result_gen;
END_RCPP
}
// MisLazyNA
Rcpp::LogicalMatrix MisLazyNA(lazyMatrixXPtr lmx);
RcppExport SEXP _lazyNumbers_MisLazyNA(SEXP lmxSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< lazyMatrixXPtr >::type lmx(lmxSEXP);
    rcpp_result_gen = Rcpp::wrap(MisLazyNA(lmx));
    return rcpp_result_gen;
END_RCPP
}
// anyLazyNA
bool anyLazyNA(lazyVectorXPtr lvx);
RcppExport SEXP _lazyNumbers_anyLazyNA(SEXP lvxSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< lazyVectorXPtr >::type lvx(lvxSEXP);
    rcpp_result_gen = Rcpp::wrap(anyLazyNA(lvx));
    return rcpp_result_gen;
END_RCPP
}
// ManyLazyNA
bool ManyLazyNA(lazyMatrixXPtr lmx);
RcppExport SEXP _lazyNumbers_ManyLazyNA(SEXP lmxSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< lazyMatrixXPtr >::type lmx(lmxSEXP);
    rcpp_result_gen = Rcpp::wrap(ManyLazyNA(lmx));
    return rcpp_result_gen;
END_RCPP
}
// lazyExact
void lazyExact(lazyVectorXPtr lvx);
RcppExport SEXP _lazyNumbers_lazyExact(SEXP lvxSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< lazyVectorXPtr >::type lvx(lvxSEXP);
    lazyExact(lvx);
    return R_NilValue;
END_RCPP
}
// MlazyExact
void MlazyExact(lazyMatrixXPtr lmx);
RcppExport SEXP _lazyNumbers_MlazyExact(SEXP lmxSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< lazyMatrixXPtr >::type lmx(lmxSEXP);
    MlazyExact(lmx);
    return R_NilValue;
END_RCPP
}
// isLazyVectorNaN_or_Inf
Rcpp::LogicalVector isLazyVectorNaN_or_Inf(lazyVectorXPtr lvx);
RcppExport SEXP _lazyNumbers_isLazyVectorNaN_or_Inf(SEXP lvxSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< lazyVectorXPtr >::type lvx(lvxSEXP);
    rcpp_result_gen = Rcpp::wrap(isLazyVectorNaN_or_Inf(lvx));
    return rcpp_result_gen;
END_RCPP
}
// isLazyMatrixNaN_or_Inf
Rcpp::LogicalMatrix isLazyMatrixNaN_or_Inf(lazyMatrixXPtr lmx);
RcppExport SEXP _lazyNumbers_isLazyMatrixNaN_or_Inf(SEXP lmxSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< lazyMatrixXPtr >::type lmx(lmxSEXP);
    rcpp_result_gen = Rcpp::wrap(isLazyMatrixNaN_or_Inf(lmx));
    return rcpp_result_gen;
END_RCPP
}
// intervals_lvx
Rcpp::List intervals_lvx(lazyVectorXPtr lvx);
RcppExport SEXP _lazyNumbers_intervals_lvx(SEXP lvxSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< lazyVectorXPtr >::type lvx(lvxSEXP);
    rcpp_result_gen = Rcpp::wrap(intervals_lvx(lvx));
    return rcpp_result_gen;
END_RCPP
}
// intervals_lmx
Rcpp::List intervals_lmx(lazyMatrixXPtr lmx);
RcppExport SEXP _lazyNumbers_intervals_lmx(SEXP lmxSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< lazyMatrixXPtr >::type lmx(lmxSEXP);
    rcpp_result_gen = Rcpp::wrap(intervals_lmx(lmx));
    return rcpp_result_gen;
END_RCPP
}
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
// nm2lmx
lazyMatrixXPtr nm2lmx(Rcpp::NumericMatrix nm);
RcppExport SEXP _lazyNumbers_nm2lmx(SEXP nmSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type nm(nmSEXP);
    rcpp_result_gen = Rcpp::wrap(nm2lmx(nm));
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
// lmx2nm
Rcpp::NumericMatrix lmx2nm(lazyMatrixXPtr lmx);
RcppExport SEXP _lazyNumbers_lmx2nm(SEXP lmxSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< lazyMatrixXPtr >::type lmx(lmxSEXP);
    rcpp_result_gen = Rcpp::wrap(lmx2nm(lmx));
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
// minus_lmx
lazyMatrixXPtr minus_lmx(lazyMatrixXPtr lmx);
RcppExport SEXP _lazyNumbers_minus_lmx(SEXP lmxSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< lazyMatrixXPtr >::type lmx(lmxSEXP);
    rcpp_result_gen = Rcpp::wrap(minus_lmx(lmx));
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
// lmx_plus_lmx
lazyMatrixXPtr lmx_plus_lmx(lazyMatrixXPtr lmx1, lazyMatrixXPtr lmx2);
RcppExport SEXP _lazyNumbers_lmx_plus_lmx(SEXP lmx1SEXP, SEXP lmx2SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< lazyMatrixXPtr >::type lmx1(lmx1SEXP);
    Rcpp::traits::input_parameter< lazyMatrixXPtr >::type lmx2(lmx2SEXP);
    rcpp_result_gen = Rcpp::wrap(lmx_plus_lmx(lmx1, lmx2));
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
// lmx_minus_lmx
lazyMatrixXPtr lmx_minus_lmx(lazyMatrixXPtr lmx1, lazyMatrixXPtr lmx2);
RcppExport SEXP _lazyNumbers_lmx_minus_lmx(SEXP lmx1SEXP, SEXP lmx2SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< lazyMatrixXPtr >::type lmx1(lmx1SEXP);
    Rcpp::traits::input_parameter< lazyMatrixXPtr >::type lmx2(lmx2SEXP);
    rcpp_result_gen = Rcpp::wrap(lmx_minus_lmx(lmx1, lmx2));
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
// lmx_cwtimes_lmx
lazyMatrixXPtr lmx_cwtimes_lmx(lazyMatrixXPtr lmx1, lazyMatrixXPtr lmx2);
RcppExport SEXP _lazyNumbers_lmx_cwtimes_lmx(SEXP lmx1SEXP, SEXP lmx2SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< lazyMatrixXPtr >::type lmx1(lmx1SEXP);
    Rcpp::traits::input_parameter< lazyMatrixXPtr >::type lmx2(lmx2SEXP);
    rcpp_result_gen = Rcpp::wrap(lmx_cwtimes_lmx(lmx1, lmx2));
    return rcpp_result_gen;
END_RCPP
}
// lmx_times_lmx
lazyMatrixXPtr lmx_times_lmx(lazyMatrixXPtr lmx1, lazyMatrixXPtr lmx2);
RcppExport SEXP _lazyNumbers_lmx_times_lmx(SEXP lmx1SEXP, SEXP lmx2SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< lazyMatrixXPtr >::type lmx1(lmx1SEXP);
    Rcpp::traits::input_parameter< lazyMatrixXPtr >::type lmx2(lmx2SEXP);
    rcpp_result_gen = Rcpp::wrap(lmx_times_lmx(lmx1, lmx2));
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
// lmx_dividedby_lmx
lazyMatrixXPtr lmx_dividedby_lmx(lazyMatrixXPtr lmx1, lazyMatrixXPtr lmx2);
RcppExport SEXP _lazyNumbers_lmx_dividedby_lmx(SEXP lmx1SEXP, SEXP lmx2SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< lazyMatrixXPtr >::type lmx1(lmx1SEXP);
    Rcpp::traits::input_parameter< lazyMatrixXPtr >::type lmx2(lmx2SEXP);
    rcpp_result_gen = Rcpp::wrap(lmx_dividedby_lmx(lmx1, lmx2));
    return rcpp_result_gen;
END_RCPP
}
// lazySum
lazyVectorXPtr lazySum(lazyVectorXPtr lvx);
RcppExport SEXP _lazyNumbers_lazySum(SEXP lvxSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< lazyVectorXPtr >::type lvx(lvxSEXP);
    rcpp_result_gen = Rcpp::wrap(lazySum(lvx));
    return rcpp_result_gen;
END_RCPP
}
// lazyProd
lazyVectorXPtr lazyProd(lazyVectorXPtr lvx);
RcppExport SEXP _lazyNumbers_lazyProd(SEXP lvxSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< lazyVectorXPtr >::type lvx(lvxSEXP);
    rcpp_result_gen = Rcpp::wrap(lazyProd(lvx));
    return rcpp_result_gen;
END_RCPP
}
// lazyCumsum
lazyVectorXPtr lazyCumsum(lazyVectorXPtr lvx);
RcppExport SEXP _lazyNumbers_lazyCumsum(SEXP lvxSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< lazyVectorXPtr >::type lvx(lvxSEXP);
    rcpp_result_gen = Rcpp::wrap(lazyCumsum(lvx));
    return rcpp_result_gen;
END_RCPP
}
// lazyCumprod
lazyVectorXPtr lazyCumprod(lazyVectorXPtr lvx);
RcppExport SEXP _lazyNumbers_lazyCumprod(SEXP lvxSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< lazyVectorXPtr >::type lvx(lvxSEXP);
    rcpp_result_gen = Rcpp::wrap(lazyCumprod(lvx));
    return rcpp_result_gen;
END_RCPP
}
// lazyConcat
lazyVectorXPtr lazyConcat(lazyVectorXPtr lvx1, lazyVectorXPtr lvx2);
RcppExport SEXP _lazyNumbers_lazyConcat(SEXP lvx1SEXP, SEXP lvx2SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< lazyVectorXPtr >::type lvx1(lvx1SEXP);
    Rcpp::traits::input_parameter< lazyVectorXPtr >::type lvx2(lvx2SEXP);
    rcpp_result_gen = Rcpp::wrap(lazyConcat(lvx1, lvx2));
    return rcpp_result_gen;
END_RCPP
}
// lazyMax
lazyVectorXPtr lazyMax(lazyVectorXPtr lvx, bool na_rm);
RcppExport SEXP _lazyNumbers_lazyMax(SEXP lvxSEXP, SEXP na_rmSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< lazyVectorXPtr >::type lvx(lvxSEXP);
    Rcpp::traits::input_parameter< bool >::type na_rm(na_rmSEXP);
    rcpp_result_gen = Rcpp::wrap(lazyMax(lvx, na_rm));
    return rcpp_result_gen;
END_RCPP
}
// lazyMin
lazyVectorXPtr lazyMin(lazyVectorXPtr lvx, bool na_rm);
RcppExport SEXP _lazyNumbers_lazyMin(SEXP lvxSEXP, SEXP na_rmSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< lazyVectorXPtr >::type lvx(lvxSEXP);
    Rcpp::traits::input_parameter< bool >::type na_rm(na_rmSEXP);
    rcpp_result_gen = Rcpp::wrap(lazyMin(lvx, na_rm));
    return rcpp_result_gen;
END_RCPP
}
// lazyRange
lazyVectorXPtr lazyRange(lazyVectorXPtr lvx, bool na_rm);
RcppExport SEXP _lazyNumbers_lazyRange(SEXP lvxSEXP, SEXP na_rmSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< lazyVectorXPtr >::type lvx(lvxSEXP);
    Rcpp::traits::input_parameter< bool >::type na_rm(na_rmSEXP);
    rcpp_result_gen = Rcpp::wrap(lazyRange(lvx, na_rm));
    return rcpp_result_gen;
END_RCPP
}
// MlazyProd
lazyVectorXPtr MlazyProd(lazyMatrixXPtr lmx);
RcppExport SEXP _lazyNumbers_MlazyProd(SEXP lmxSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< lazyMatrixXPtr >::type lmx(lmxSEXP);
    rcpp_result_gen = Rcpp::wrap(MlazyProd(lmx));
    return rcpp_result_gen;
END_RCPP
}
// MlazySum
lazyVectorXPtr MlazySum(lazyMatrixXPtr lmx);
RcppExport SEXP _lazyNumbers_MlazySum(SEXP lmxSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< lazyMatrixXPtr >::type lmx(lmxSEXP);
    rcpp_result_gen = Rcpp::wrap(MlazySum(lmx));
    return rcpp_result_gen;
END_RCPP
}
// MlazyMax
lazyVectorXPtr MlazyMax(lazyMatrixXPtr lmx, bool na_rm);
RcppExport SEXP _lazyNumbers_MlazyMax(SEXP lmxSEXP, SEXP na_rmSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< lazyMatrixXPtr >::type lmx(lmxSEXP);
    Rcpp::traits::input_parameter< bool >::type na_rm(na_rmSEXP);
    rcpp_result_gen = Rcpp::wrap(MlazyMax(lmx, na_rm));
    return rcpp_result_gen;
END_RCPP
}
// MlazyMin
lazyVectorXPtr MlazyMin(lazyMatrixXPtr lmx, bool na_rm);
RcppExport SEXP _lazyNumbers_MlazyMin(SEXP lmxSEXP, SEXP na_rmSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< lazyMatrixXPtr >::type lmx(lmxSEXP);
    Rcpp::traits::input_parameter< bool >::type na_rm(na_rmSEXP);
    rcpp_result_gen = Rcpp::wrap(MlazyMin(lmx, na_rm));
    return rcpp_result_gen;
END_RCPP
}
// MlazyRange
lazyVectorXPtr MlazyRange(lazyMatrixXPtr lmx, bool na_rm);
RcppExport SEXP _lazyNumbers_MlazyRange(SEXP lmxSEXP, SEXP na_rmSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< lazyMatrixXPtr >::type lmx(lmxSEXP);
    Rcpp::traits::input_parameter< bool >::type na_rm(na_rmSEXP);
    rcpp_result_gen = Rcpp::wrap(MlazyRange(lmx, na_rm));
    return rcpp_result_gen;
END_RCPP
}
// lazyPower
lazyVectorXPtr lazyPower(lazyVectorXPtr lvx, int alpha);
RcppExport SEXP _lazyNumbers_lazyPower(SEXP lvxSEXP, SEXP alphaSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< lazyVectorXPtr >::type lvx(lvxSEXP);
    Rcpp::traits::input_parameter< int >::type alpha(alphaSEXP);
    rcpp_result_gen = Rcpp::wrap(lazyPower(lvx, alpha));
    return rcpp_result_gen;
END_RCPP
}
// MlazyPower
lazyMatrixXPtr MlazyPower(lazyMatrixXPtr lmx, int alpha);
RcppExport SEXP _lazyNumbers_MlazyPower(SEXP lmxSEXP, SEXP alphaSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< lazyMatrixXPtr >::type lmx(lmxSEXP);
    Rcpp::traits::input_parameter< int >::type alpha(alphaSEXP);
    rcpp_result_gen = Rcpp::wrap(MlazyPower(lmx, alpha));
    return rcpp_result_gen;
END_RCPP
}
// lazyAbs
lazyVectorXPtr lazyAbs(lazyVectorXPtr lvx);
RcppExport SEXP _lazyNumbers_lazyAbs(SEXP lvxSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< lazyVectorXPtr >::type lvx(lvxSEXP);
    rcpp_result_gen = Rcpp::wrap(lazyAbs(lvx));
    return rcpp_result_gen;
END_RCPP
}
// MlazyAbs
lazyMatrixXPtr MlazyAbs(lazyMatrixXPtr lmx);
RcppExport SEXP _lazyNumbers_MlazyAbs(SEXP lmxSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< lazyMatrixXPtr >::type lmx(lmxSEXP);
    rcpp_result_gen = Rcpp::wrap(MlazyAbs(lmx));
    return rcpp_result_gen;
END_RCPP
}
// lazyExtract
lazyVectorXPtr lazyExtract(lazyVectorXPtr lvx, Rcpp::IntegerVector indices);
RcppExport SEXP _lazyNumbers_lazyExtract(SEXP lvxSEXP, SEXP indicesSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< lazyVectorXPtr >::type lvx(lvxSEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerVector >::type indices(indicesSEXP);
    rcpp_result_gen = Rcpp::wrap(lazyExtract(lvx, indices));
    return rcpp_result_gen;
END_RCPP
}
// MlazyExtract
lazyMatrixXPtr MlazyExtract(lazyMatrixXPtr lmx, Rcpp::IntegerMatrix indices, int m, int n);
RcppExport SEXP _lazyNumbers_MlazyExtract(SEXP lmxSEXP, SEXP indicesSEXP, SEXP mSEXP, SEXP nSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< lazyMatrixXPtr >::type lmx(lmxSEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerMatrix >::type indices(indicesSEXP);
    Rcpp::traits::input_parameter< int >::type m(mSEXP);
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    rcpp_result_gen = Rcpp::wrap(MlazyExtract(lmx, indices, m, n));
    return rcpp_result_gen;
END_RCPP
}
// lazyReplace
lazyVectorXPtr lazyReplace(lazyVectorXPtr lvx1, Rcpp::IntegerVector indices, lazyVectorXPtr lvx2);
RcppExport SEXP _lazyNumbers_lazyReplace(SEXP lvx1SEXP, SEXP indicesSEXP, SEXP lvx2SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< lazyVectorXPtr >::type lvx1(lvx1SEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerVector >::type indices(indicesSEXP);
    Rcpp::traits::input_parameter< lazyVectorXPtr >::type lvx2(lvx2SEXP);
    rcpp_result_gen = Rcpp::wrap(lazyReplace(lvx1, indices, lvx2));
    return rcpp_result_gen;
END_RCPP
}
// lazyColumnMatrix
lazyMatrixXPtr lazyColumnMatrix(lazyVectorXPtr lvx);
RcppExport SEXP _lazyNumbers_lazyColumnMatrix(SEXP lvxSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< lazyVectorXPtr >::type lvx(lvxSEXP);
    rcpp_result_gen = Rcpp::wrap(lazyColumnMatrix(lvx));
    return rcpp_result_gen;
END_RCPP
}
// lazyRowMatrix
lazyMatrixXPtr lazyRowMatrix(lazyVectorXPtr lvx);
RcppExport SEXP _lazyNumbers_lazyRowMatrix(SEXP lvxSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< lazyVectorXPtr >::type lvx(lvxSEXP);
    rcpp_result_gen = Rcpp::wrap(lazyRowMatrix(lvx));
    return rcpp_result_gen;
END_RCPP
}
// lazyRbind
lazyMatrixXPtr lazyRbind(lazyMatrixXPtr lmx1, lazyMatrixXPtr lmx2);
RcppExport SEXP _lazyNumbers_lazyRbind(SEXP lmx1SEXP, SEXP lmx2SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< lazyMatrixXPtr >::type lmx1(lmx1SEXP);
    Rcpp::traits::input_parameter< lazyMatrixXPtr >::type lmx2(lmx2SEXP);
    rcpp_result_gen = Rcpp::wrap(lazyRbind(lmx1, lmx2));
    return rcpp_result_gen;
END_RCPP
}
// lazyCbind
lazyMatrixXPtr lazyCbind(lazyMatrixXPtr lmx1, lazyMatrixXPtr lmx2);
RcppExport SEXP _lazyNumbers_lazyCbind(SEXP lmx1SEXP, SEXP lmx2SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< lazyMatrixXPtr >::type lmx1(lmx1SEXP);
    Rcpp::traits::input_parameter< lazyMatrixXPtr >::type lmx2(lmx2SEXP);
    rcpp_result_gen = Rcpp::wrap(lazyCbind(lmx1, lmx2));
    return rcpp_result_gen;
END_RCPP
}
// lazyDeterminant
lazyVectorXPtr lazyDeterminant(lazyMatrixXPtr lmx);
RcppExport SEXP _lazyNumbers_lazyDeterminant(SEXP lmxSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< lazyMatrixXPtr >::type lmx(lmxSEXP);
    rcpp_result_gen = Rcpp::wrap(lazyDeterminant(lmx));
    return rcpp_result_gen;
END_RCPP
}
// lazyInverse
lazyMatrixXPtr lazyInverse(lazyMatrixXPtr lmx);
RcppExport SEXP _lazyNumbers_lazyInverse(SEXP lmxSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< lazyMatrixXPtr >::type lmx(lmxSEXP);
    rcpp_result_gen = Rcpp::wrap(lazyInverse(lmx));
    return rcpp_result_gen;
END_RCPP
}
// lazyTranspose
lazyMatrixXPtr lazyTranspose(lazyMatrixXPtr lmx);
RcppExport SEXP _lazyNumbers_lazyTranspose(SEXP lmxSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< lazyMatrixXPtr >::type lmx(lmxSEXP);
    rcpp_result_gen = Rcpp::wrap(lazyTranspose(lmx));
    return rcpp_result_gen;
END_RCPP
}
// lazyFlatten
lazyVectorXPtr lazyFlatten(lazyMatrixXPtr lmx);
RcppExport SEXP _lazyNumbers_lazyFlatten(SEXP lmxSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< lazyMatrixXPtr >::type lmx(lmxSEXP);
    rcpp_result_gen = Rcpp::wrap(lazyFlatten(lmx));
    return rcpp_result_gen;
END_RCPP
}
// lazyCompare
Rcpp::LogicalVector lazyCompare(lazyVectorXPtr lvx1, lazyVectorXPtr lvx2, Rcpp::String r);
RcppExport SEXP _lazyNumbers_lazyCompare(SEXP lvx1SEXP, SEXP lvx2SEXP, SEXP rSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< lazyVectorXPtr >::type lvx1(lvx1SEXP);
    Rcpp::traits::input_parameter< lazyVectorXPtr >::type lvx2(lvx2SEXP);
    Rcpp::traits::input_parameter< Rcpp::String >::type r(rSEXP);
    rcpp_result_gen = Rcpp::wrap(lazyCompare(lvx1, lvx2, r));
    return rcpp_result_gen;
END_RCPP
}
// lazyDiagonal
lazyVectorXPtr lazyDiagonal(lazyMatrixXPtr lmx);
RcppExport SEXP _lazyNumbers_lazyDiagonal(SEXP lmxSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< lazyMatrixXPtr >::type lmx(lmxSEXP);
    rcpp_result_gen = Rcpp::wrap(lazyDiagonal(lmx));
    return rcpp_result_gen;
END_RCPP
}
// lazyReplaceDiagonal
lazyMatrixXPtr lazyReplaceDiagonal(lazyMatrixXPtr lmx, lazyVectorXPtr lvx);
RcppExport SEXP _lazyNumbers_lazyReplaceDiagonal(SEXP lmxSEXP, SEXP lvxSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< lazyMatrixXPtr >::type lmx(lmxSEXP);
    Rcpp::traits::input_parameter< lazyVectorXPtr >::type lvx(lvxSEXP);
    rcpp_result_gen = Rcpp::wrap(lazyReplaceDiagonal(lmx, lvx));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_lazyNumbers_lazyNA", (DL_FUNC) &_lazyNumbers_lazyNA, 0},
    {"_lazyNumbers_lazyNAomit", (DL_FUNC) &_lazyNumbers_lazyNAomit, 1},
    {"_lazyNumbers_isLazyNA", (DL_FUNC) &_lazyNumbers_isLazyNA, 1},
    {"_lazyNumbers_MisLazyNA", (DL_FUNC) &_lazyNumbers_MisLazyNA, 1},
    {"_lazyNumbers_anyLazyNA", (DL_FUNC) &_lazyNumbers_anyLazyNA, 1},
    {"_lazyNumbers_ManyLazyNA", (DL_FUNC) &_lazyNumbers_ManyLazyNA, 1},
    {"_lazyNumbers_lazyExact", (DL_FUNC) &_lazyNumbers_lazyExact, 1},
    {"_lazyNumbers_MlazyExact", (DL_FUNC) &_lazyNumbers_MlazyExact, 1},
    {"_lazyNumbers_isLazyVectorNaN_or_Inf", (DL_FUNC) &_lazyNumbers_isLazyVectorNaN_or_Inf, 1},
    {"_lazyNumbers_isLazyMatrixNaN_or_Inf", (DL_FUNC) &_lazyNumbers_isLazyMatrixNaN_or_Inf, 1},
    {"_lazyNumbers_intervals_lvx", (DL_FUNC) &_lazyNumbers_intervals_lvx, 1},
    {"_lazyNumbers_intervals_lmx", (DL_FUNC) &_lazyNumbers_intervals_lmx, 1},
    {"_lazyNumbers_nv2lvx", (DL_FUNC) &_lazyNumbers_nv2lvx, 1},
    {"_lazyNumbers_nm2lmx", (DL_FUNC) &_lazyNumbers_nm2lmx, 1},
    {"_lazyNumbers_lvx2nv", (DL_FUNC) &_lazyNumbers_lvx2nv, 1},
    {"_lazyNumbers_lmx2nm", (DL_FUNC) &_lazyNumbers_lmx2nm, 1},
    {"_lazyNumbers_minus_lvx", (DL_FUNC) &_lazyNumbers_minus_lvx, 1},
    {"_lazyNumbers_minus_lmx", (DL_FUNC) &_lazyNumbers_minus_lmx, 1},
    {"_lazyNumbers_lvx_plus_lvx", (DL_FUNC) &_lazyNumbers_lvx_plus_lvx, 2},
    {"_lazyNumbers_lmx_plus_lmx", (DL_FUNC) &_lazyNumbers_lmx_plus_lmx, 2},
    {"_lazyNumbers_lvx_minus_lvx", (DL_FUNC) &_lazyNumbers_lvx_minus_lvx, 2},
    {"_lazyNumbers_lmx_minus_lmx", (DL_FUNC) &_lazyNumbers_lmx_minus_lmx, 2},
    {"_lazyNumbers_lvx_times_lvx", (DL_FUNC) &_lazyNumbers_lvx_times_lvx, 2},
    {"_lazyNumbers_lmx_cwtimes_lmx", (DL_FUNC) &_lazyNumbers_lmx_cwtimes_lmx, 2},
    {"_lazyNumbers_lmx_times_lmx", (DL_FUNC) &_lazyNumbers_lmx_times_lmx, 2},
    {"_lazyNumbers_lvx_dividedby_lvx", (DL_FUNC) &_lazyNumbers_lvx_dividedby_lvx, 2},
    {"_lazyNumbers_lmx_dividedby_lmx", (DL_FUNC) &_lazyNumbers_lmx_dividedby_lmx, 2},
    {"_lazyNumbers_lazySum", (DL_FUNC) &_lazyNumbers_lazySum, 1},
    {"_lazyNumbers_lazyProd", (DL_FUNC) &_lazyNumbers_lazyProd, 1},
    {"_lazyNumbers_lazyCumsum", (DL_FUNC) &_lazyNumbers_lazyCumsum, 1},
    {"_lazyNumbers_lazyCumprod", (DL_FUNC) &_lazyNumbers_lazyCumprod, 1},
    {"_lazyNumbers_lazyConcat", (DL_FUNC) &_lazyNumbers_lazyConcat, 2},
    {"_lazyNumbers_lazyMax", (DL_FUNC) &_lazyNumbers_lazyMax, 2},
    {"_lazyNumbers_lazyMin", (DL_FUNC) &_lazyNumbers_lazyMin, 2},
    {"_lazyNumbers_lazyRange", (DL_FUNC) &_lazyNumbers_lazyRange, 2},
    {"_lazyNumbers_MlazyProd", (DL_FUNC) &_lazyNumbers_MlazyProd, 1},
    {"_lazyNumbers_MlazySum", (DL_FUNC) &_lazyNumbers_MlazySum, 1},
    {"_lazyNumbers_MlazyMax", (DL_FUNC) &_lazyNumbers_MlazyMax, 2},
    {"_lazyNumbers_MlazyMin", (DL_FUNC) &_lazyNumbers_MlazyMin, 2},
    {"_lazyNumbers_MlazyRange", (DL_FUNC) &_lazyNumbers_MlazyRange, 2},
    {"_lazyNumbers_lazyPower", (DL_FUNC) &_lazyNumbers_lazyPower, 2},
    {"_lazyNumbers_MlazyPower", (DL_FUNC) &_lazyNumbers_MlazyPower, 2},
    {"_lazyNumbers_lazyAbs", (DL_FUNC) &_lazyNumbers_lazyAbs, 1},
    {"_lazyNumbers_MlazyAbs", (DL_FUNC) &_lazyNumbers_MlazyAbs, 1},
    {"_lazyNumbers_lazyExtract", (DL_FUNC) &_lazyNumbers_lazyExtract, 2},
    {"_lazyNumbers_MlazyExtract", (DL_FUNC) &_lazyNumbers_MlazyExtract, 4},
    {"_lazyNumbers_lazyReplace", (DL_FUNC) &_lazyNumbers_lazyReplace, 3},
    {"_lazyNumbers_lazyColumnMatrix", (DL_FUNC) &_lazyNumbers_lazyColumnMatrix, 1},
    {"_lazyNumbers_lazyRowMatrix", (DL_FUNC) &_lazyNumbers_lazyRowMatrix, 1},
    {"_lazyNumbers_lazyRbind", (DL_FUNC) &_lazyNumbers_lazyRbind, 2},
    {"_lazyNumbers_lazyCbind", (DL_FUNC) &_lazyNumbers_lazyCbind, 2},
    {"_lazyNumbers_lazyDeterminant", (DL_FUNC) &_lazyNumbers_lazyDeterminant, 1},
    {"_lazyNumbers_lazyInverse", (DL_FUNC) &_lazyNumbers_lazyInverse, 1},
    {"_lazyNumbers_lazyTranspose", (DL_FUNC) &_lazyNumbers_lazyTranspose, 1},
    {"_lazyNumbers_lazyFlatten", (DL_FUNC) &_lazyNumbers_lazyFlatten, 1},
    {"_lazyNumbers_lazyCompare", (DL_FUNC) &_lazyNumbers_lazyCompare, 3},
    {"_lazyNumbers_lazyDiagonal", (DL_FUNC) &_lazyNumbers_lazyDiagonal, 1},
    {"_lazyNumbers_lazyReplaceDiagonal", (DL_FUNC) &_lazyNumbers_lazyReplaceDiagonal, 2},
    {NULL, NULL, 0}
};

RcppExport void R_init_lazyNumbers(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
