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
  const size_t n1 = lv1.size();
  const size_t n2 = lv2.size();
  lazyVector lv;
  if(n1 == n2) {
    lv.reserve(n1);
    for(size_t i = 0; i < n1; i++) {
      lv.emplace_back(lv1[i] + lv2[i]);
    }
  } else if(n1 == 1) {
    lv.reserve(n2);
    for(size_t i = 0; i < n2; i++) {
      lv.emplace_back(lv1[0] + lv2[i]);
    }
  } else if(n2 == 1) {
    lv.reserve(n1);
    for(size_t i = 0; i < n1; i++) {
      lv.emplace_back(lv1[i] + lv2[0]);
    }
  } else {
    Rcpp::stop("Incompatible lengths.");
  }
  return lazyVectorXPtr(new lazyVector(lv), false);
}

// [[Rcpp::export]]
lazyVectorXPtr lvx_minus_lvx(lazyVectorXPtr lvx1, lazyVectorXPtr lvx2) {
  lazyVector lv1 = *(lvx1.get());
  lazyVector lv2 = *(lvx2.get());
  const size_t n1 = lv1.size();
  const size_t n2 = lv2.size();
  lazyVector lv;
  if(n1 == n2) {
    lv.reserve(n1);
    for(size_t i = 0; i < n1; i++) {
      lv.emplace_back(lv1[i] - lv2[i]);
    }
  } else if(n1 == 1) {
    lv.reserve(n2);
    for(size_t i = 0; i < n2; i++) {
      lv.emplace_back(lv1[0] - lv2[i]);
    }
  } else if(n2 == 1) {
    lv.reserve(n1);
    for(size_t i = 0; i < n1; i++) {
      lv.emplace_back(lv1[i] - lv2[0]);
    }
  } else {
    Rcpp::stop("Incompatible lengths.");
  }
  return lazyVectorXPtr(new lazyVector(lv), false);
}

// [[Rcpp::export]]
lazyVectorXPtr lvx_times_lvx(lazyVectorXPtr lvx1, lazyVectorXPtr lvx2) {
  lazyVector lv1 = *(lvx1.get());
  lazyVector lv2 = *(lvx2.get());
  const size_t n1 = lv1.size();
  const size_t n2 = lv2.size();
  lazyVector lv;
  if(n1 == n2) {
    lv.reserve(n1);
    for(size_t i = 0; i < n1; i++) {
      lv.emplace_back(lv1[i] * lv2[i]);
    }
  } else if(n1 == 1) {
    lv.reserve(n2);
    for(size_t i = 0; i < n2; i++) {
      lv.emplace_back(lv1[0] * lv2[i]);
    }
  } else if(n2 == 1) {
    lv.reserve(n1);
    for(size_t i = 0; i < n1; i++) {
      lv.emplace_back(lv1[i] * lv2[0]);
    }
  } else {
    Rcpp::stop("Incompatible lengths.");
  }
  return lazyVectorXPtr(new lazyVector(lv), false);
}

// [[Rcpp::export]]
lazyVectorXPtr lvx_dividedby_lvx(lazyVectorXPtr lvx1, lazyVectorXPtr lvx2) {
  lazyVector lv1 = *(lvx1.get());
  lazyVector lv2 = *(lvx2.get());
  const size_t n1 = lv1.size();
  const size_t n2 = lv2.size();
  lazyVector lv;
  if(n1 == n2) {
    lv.reserve(n1);
    for(size_t i = 0; i < n1; i++) {
      if(lv2[i] == 0) {
        Rcpp::stop("Division by zero.");
      }
      lv.emplace_back(lv1[i] / lv2[i]);
    }
  } else if(n1 == 1) {
    lv.reserve(n2);
    for(size_t i = 0; i < n2; i++) {
      if(lv2[i] == 0) {
        Rcpp::stop("Division by zero.");
      }
      lv.emplace_back(lv1[0] / lv2[i]);
    }
  } else if(n2 == 1) {
    if(lv2[0] == 0) {
      Rcpp::stop("Division by zero.");
    }
    lv.reserve(n1);
    for(size_t i = 0; i < n1; i++) {
      lv.emplace_back(lv1[i] / lv2[0]);
    }
  } else {
    Rcpp::stop("Incompatible lengths.");
  }
  return lazyVectorXPtr(new lazyVector(lv), false);
}

// [[Rcpp::export]]
lazyVectorXPtr lazySum(lazyVectorXPtr lvx) {
  lazyVector lvin = *(lvx.get());
  const size_t n = lvin.size();
  LN sum(0);
  for(size_t i = 0; i < n; i++) {
    sum += lvin[i];
  }
  lazyVector lv = {sum};
  return lazyVectorXPtr(new lazyVector(lv), false);
}

// [[Rcpp::export]]
lazyVectorXPtr lazyProd(lazyVectorXPtr lvx) {
  lazyVector lvin = *(lvx.get());
  const size_t n = lvin.size();
  LN prod(1);
  for(size_t i = 0; i < n; i++) {
    prod *= lvin[i];
  }
  lazyVector lv = {prod};
  return lazyVectorXPtr(new lazyVector(lv), false);
}

// [[Rcpp::export]]
lazyVectorXPtr lazyCumsum(lazyVectorXPtr lvx) {
  lazyVector lvin = *(lvx.get());
  const size_t n = lvin.size();
  lazyVector lv(n);
  LN sum(0);
  for(size_t i = 0; i < n; i++) {
    sum += lvin[i];
    lv[i] = sum;
  }
  return lazyVectorXPtr(new lazyVector(lv), false);
}

// [[Rcpp::export]]
lazyVectorXPtr lazyCumprod(lazyVectorXPtr lvx) {
  lazyVector lvin = *(lvx.get());
  const size_t n = lvin.size();
  lazyVector lv(n);
  LN prod(1);
  for(size_t i = 0; i < n; i++) {
    prod *= lvin[i];
    lv[i] = prod;
  }
  return lazyVectorXPtr(new lazyVector(lv), false);
}
