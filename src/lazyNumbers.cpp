#include "lazyNumbers_types.h"

// [[Rcpp::export]]
lazyVectorXPtr nv2lvx(Rcpp::NumericVector nv) {
  const size_t n = nv.size();
  lazyVector lv(n);
  for(size_t i = 0; i < n; i++) {
    lv[i] = LN(nv(i));
  }
  return lazyVectorXPtr(new lazyVector(lv), false);
}

// [[Rcpp::export]]
Rcpp::NumericVector lvx2nv(lazyVectorXPtr lvx) {
  lazyVector lv = *(lvx.get());
  const size_t n = lv.size();
  Rcpp::NumericVector nv(n);
  for(size_t i = 0; i < n; i++) {
    nv(i) = CGAL::to_double<LN>(lv[i]);
  }
  return nv;
}

// [[Rcpp::export]]
lazyVectorXPtr minus_lvx(lazyVectorXPtr lvx) {
  lazyVector lvin = *(lvx.get());
  const size_t n = lvin.size();
  lazyVector lv(n);
  for(size_t i = 0; i < n; i++) {
    lv[i] = - lvin[i];
  }
  return lazyVectorXPtr(new lazyVector(lv), false);
}

// [[Rcpp::export]]
lazyVectorXPtr lvx_plus_lvx(lazyVectorXPtr lvx1, lazyVectorXPtr lvx2) {
  lazyVector lv1 = *(lvx1.get());
  lazyVector lv2 = *(lvx2.get());
  const size_t n = lv1.size();
  lazyVector lv(n);
  for(size_t i = 0; i < n; i++) {
    lv[i] = lv1[i] + lv2[i];
  }
  return lazyVectorXPtr(new lazyVector(lv), false);
}

// [[Rcpp::export]]
lazyVectorXPtr lvx_minus_lvx(lazyVectorXPtr lvx1, lazyVectorXPtr lvx2) {
  lazyVector lv1 = *(lvx1.get());
  lazyVector lv2 = *(lvx2.get());
  const size_t n = lv1.size();
  lazyVector lv(n);
  for(size_t i = 0; i < n; i++) {
    lv[i] = lv1[i] - lv2[i];
  }
  return lazyVectorXPtr(new lazyVector(lv), false);
}

// [[Rcpp::export]]
lazyVectorXPtr lvx_times_lvx(lazyVectorXPtr lvx1, lazyVectorXPtr lvx2) {
  lazyVector lv1 = *(lvx1.get());
  lazyVector lv2 = *(lvx2.get());
  const size_t n = lv1.size();
  lazyVector lv(n);
  for(size_t i = 0; i < n; i++) {
    lv[i] = lv1[i] * lv2[i];
  }
  return lazyVectorXPtr(new lazyVector(lv), false);
}

// [[Rcpp::export]]
lazyVectorXPtr lvx_dividedby_lvx(lazyVectorXPtr lvx1, lazyVectorXPtr lvx2) {
  lazyVector lv1 = *(lvx1.get());
  lazyVector lv2 = *(lvx2.get());
  const size_t n = lv1.size();
  lazyVector lv(n);
  for(size_t i = 0; i < n; i++) {
    lv[i] = lv1[i] / lv2[i];
  }
  return lazyVectorXPtr(new lazyVector(lv), false);
}