#include "lazyNumbers_types.h"

namespace Eigen {

template<> struct NumTraits<lazyScalar> : GenericNumTraits<lazyScalar>
{
  typedef lazyScalar Real;
  typedef lazyScalar NonInteger;
  typedef lazyScalar Nested;
  
  enum {
    IsComplex = 0,
    IsInteger = 0,
    IsSigned = 1,
    RequireInitialization = 1,
    ReadCost = 1,
    AddCost = 3,
    MulCost = 3
  };
};

}

namespace std {

lazyScalar& operator+=(lazyScalar& self, const lazyScalar& other) {
  if(self && other) {
    self = lazyScalar(*self + *other);
  } else {
    self = std::nullopt; 
  }
  return self;
}

lazyScalar operator+(const lazyScalar& lhs, const lazyScalar& rhs) {
  lazyScalar that = lhs;
  that += rhs;
  return that;
}

lazyScalar& operator-=(lazyScalar& self, const lazyScalar& other) {
  if(self && other) {
    self = lazyScalar(*self - *other);
  } else {
    self = std::nullopt; 
  }
  return self;
}

lazyScalar operator-(const lazyScalar& lhs, const lazyScalar& rhs) {
  lazyScalar that = lhs;
  that -= rhs;
  return that;
}

lazyScalar operator-(const lazyScalar& x) {
  lazyScalar that = x;
  const lazyScalar zero(lazyNumber(0));
  return zero - x;
}

lazyScalar& operator*=(lazyScalar& self, const lazyScalar& other) {
  if(self && other) {
    self = lazyScalar(*self * *other);
  } else {
    self = std::nullopt; 
  }
  return self;
}

lazyScalar operator*(const lazyScalar& lhs, const lazyScalar& rhs) {
  lazyScalar that = lhs;
  that *= rhs;
  return that;
}

lazyScalar& operator/=(lazyScalar& self, const lazyScalar& other) {
  if(self && other) {
    self = lazyScalar(*self / *other);
  } else {
    self = std::nullopt; 
  }
  return self;
}

lazyScalar operator/(const lazyScalar& lhs, const lazyScalar& rhs) {
  lazyScalar that = lhs;
  that /= rhs;
  return that;
}

lazyScalar max(const lazyScalar& x, const lazyScalar& y) {
  if(x && y) {
    return max(*x, *y);
  } else {
    return std::nullopt;
  }
}

lazyScalar min(const lazyScalar& x, const lazyScalar& y) {
  if(x && y) {
    return min(*x, *y);
  } else {
    return std::nullopt;
  }
}

lazyScalar abs(const lazyScalar& x) {
  if(x) {
    return CGAL::abs(*x);
  } else {
    return std::nullopt;
  }
}

}

// [[Rcpp::export]]
lazyMatrixXPtr lazyVector2lazyMatrix(lazyVectorXPtr lvx, int nrow, int ncol) {
  lazyVector lv = *(lvx.get());
  int n = lv.size();
  if(nrow * ncol != n) {
    Rcpp::stop("Incompatible dimensions");
  }
  lazyMatrix lm = Eigen::Map<lazyMatrix>(lv.data(), nrow, ncol);
  return lazyMatrixXPtr(new lazyMatrix(lm), false);
}

// [[Rcpp::export]]
lazyVectorXPtr lazyNA() {
  return lazyVectorXPtr(new lazyVector({std::nullopt}), false);
}

lazyVector lazyNAomit0(lazyVector lv) {
  lazyVector out;
  for(size_t i = 0; i < lv.size(); i++) {
    lazyScalar x = lv[i];
    if(x) {
      out.push_back(x);
    }
  }
  return out;
}

// [[Rcpp::export]]
lazyVectorXPtr lazyNAomit(lazyVectorXPtr lvx) {
  lazyVector lv = *(lvx.get());
  lazyVector lvout = lazyNAomit0(lv);
  lazyVectorXPtr lvxout(new lazyVector(lvout), false);
  int l = lvout.size();
  lvxout.attr("length") = l;
  return lvxout;
}

// [[Rcpp::export]]
Rcpp::LogicalVector isLazyNA(lazyVectorXPtr lvx) {
  lazyVector lv = *(lvx.get());
  const size_t n = lv.size();
  Rcpp::LogicalVector out(n);
  for(size_t i = 0; i < n; i++) {
    if(lv[i]) {
      out(i) = false;
    } else {
      out(i) = true;
    }
  }
  return out;
}

// [[Rcpp::export]]
Rcpp::LogicalMatrix MisLazyNA(lazyMatrixXPtr lmx) {
  lazyMatrix lm = *(lmx.get());
  const size_t m = lm.rows();
  const size_t n = lm.cols();
  Rcpp::LogicalMatrix out(m, n);
  for(size_t j = 0; j < n; j++) {
    Rcpp::LogicalVector colj(m);
    for(size_t i = 0; i < m; i++) {
      colj(i) = lm.coeff(i, j) ? false : true;
    }
    out(Rcpp::_, j) = colj;
  }
  return out;
}

// [[Rcpp::export]]
bool anyLazyNA(lazyVectorXPtr lvx) {
  lazyVector lv = *(lvx.get());
  for(size_t i = 0; i < lv.size(); i++) {
    if(!lv[i]) {
      return true;
    }
  }
  return false;
}

// [[Rcpp::export]]
bool ManyLazyNA(lazyMatrixXPtr lmx) {
  lazyMatrix lm = *(lmx.get());
  for(size_t k = 0; k < lm.size(); k++) {
    if(!*(lm.data() + k)) {
      return true;
    }
  }
  return false;
}

// [[Rcpp::export]]
void lazyExact(lazyVectorXPtr lvx) {
  lazyVector lv = *(lvx.get());
  for(size_t i = 0; i < lv.size(); i++) {
    lazyScalar x = lv[i];
    if(x) {
      Quotient q = (*x).exact();
    }
  }
}

// [[Rcpp::export]]
void MlazyExact(lazyMatrixXPtr lmx) {
  lazyMatrix lm = *(lmx.get());
  for(size_t k = 0; k < lm.size(); k++) {
    lazyScalar x = *(lm.data() + k);
    if(x) {
      Quotient q = (*x).exact();
    }
  }
  // for(size_t i = 0; i < lm.rows(); i++) {
  //   for(size_t j = 0; j < lm.cols(); j++) {
  //     lazyScalar x = lm.coeff(i, j);
  //     if(x) {
  //       Quotient q = (*x).exact();
  //     }
  //   }
  // }
}

bool isLazyNaN_or_Inf(lazyNumber x) {
  std::pair<double, double> interval1 = CGAL::to_interval(x);
  bool out = false;
  if(isinf(interval1.first) && isinf(interval1.second)) {
    // lazyNumber invx = lazyNumber(1) / x;
    // std::pair<double, double> interval2 = CGAL::to_interval(invx);
    // if(isinf(interval2.first) && isinf(interval2.second)) {
      out = true;
    // }
  }
  return out;
}

// [[Rcpp::export]]
Rcpp::LogicalVector isLazyVectorNaN_or_Inf(lazyVectorXPtr lvx) {
  lazyVector lv = *(lvx.get());
  const size_t n = lv.size();
  Rcpp::LogicalVector out(n);
  for(size_t i = 0; i < n; i++) {
    lazyScalar x = lv[i];
    if(x && isLazyNaN_or_Inf(*x)) {
      out(i) = true;
    } else {
      out(i) = false;
    }
  }
  return out;
}

// [[Rcpp::export]]
Rcpp::LogicalMatrix isLazyMatrixNaN_or_Inf(lazyMatrixXPtr lmx) {
  lazyMatrix lm = *(lmx.get());
  const size_t m = lm.rows();
  const size_t n = lm.cols();
  Rcpp::LogicalMatrix out(m, n);
  for(size_t j = 0; j < n; j++) {
    Rcpp::LogicalVector colj(m);
    for(size_t i = 0; i < m; i++) {
      lazyScalar x = lm.coeff(i, j);
      colj(i) = x && isLazyNaN_or_Inf(*x) ? true : false;
    }
    out(Rcpp::_, j) = colj;
  }
  return out;
}

// [[Rcpp::export]]
Rcpp::List intervals_lvx(lazyVectorXPtr lvx) {
  lazyVector lv = *(lvx.get());
  const size_t n = lv.size();
  Rcpp::NumericVector inf(n);
  Rcpp::NumericVector sup(n);
  for(size_t i = 0; i < n; i++) {
    lazyScalar x = lv[i];
    if(x) {
      CGAL::Interval_nt<true> interval = (*x).interval();
      inf(i) = interval.inf();
      sup(i) = interval.sup();
    } else {
      inf(i) = Rcpp::NumericVector::get_na();
      sup(i) = Rcpp::NumericVector::get_na();
    }
  }
  return Rcpp::List::create(Rcpp::Named("inf") = inf, Rcpp::Named("sup") = sup);
}

// [[Rcpp::export]]
Rcpp::List intervals_lmx(lazyMatrixXPtr lmx) {
  lazyMatrix lm = *(lmx.get());
  const size_t m = lm.rows();
  const size_t n = lm.cols();
  Rcpp::NumericMatrix inf(m, n);
  Rcpp::NumericMatrix sup(m, n);
  for(size_t i = 0; i < m; i++) {
    for(size_t j = 0; j < n; j++) {
      lazyScalar x = lm.coeff(i, j);
      if(x) {
        CGAL::Interval_nt<true> interval = (*x).interval();
        inf(i, j) = interval.inf();
        sup(i, j) = interval.sup();
      } else {
        inf(i, j) = Rcpp::NumericVector::get_na();
        sup(i, j) = Rcpp::NumericVector::get_na();
      }
    }
  }
  return Rcpp::List::create(Rcpp::Named("inf") = inf, Rcpp::Named("sup") = sup);
}

bool isNaN(Rcpp::NumericVector x, size_t index) {
  return Rcpp::is_nan(x[Rcpp::Range(index, index)])[0];
}

// [[Rcpp::export]]
lazyVectorXPtr nv2lvx(Rcpp::NumericVector nv) {
  const size_t n = nv.size();
  lazyVector lv(n);
  for(size_t i = 0; i < n; i++) {
    if(isinf(nv(i))) { 
      if(nv(i) > 0) {
        lazyNumber x(Quotient(1) / Quotient(0));
        lv[i] = lazyScalar(x);
      } else {
        lazyNumber x(Quotient(-1) / Quotient(0));
        lv[i] = lazyScalar(x);
      }
    } else if(isNaN(nv, i)) {
      lazyNumber x(Quotient(0) / Quotient(0));
      lv[i] = lazyScalar(x);
    } else if(Rcpp::NumericVector::is_na(nv(i))) {
      lv[i] = std::nullopt;
    } else {
      lv[i] = lazyScalar(nv(i));
    }
  }
  return lazyVectorXPtr(new lazyVector(lv), false);
}

// [[Rcpp::export]]
lazyMatrixXPtr nm2lmx(Rcpp::NumericMatrix nm) {
  const size_t nrow = nm.nrow();
  const size_t ncol = nm.ncol();
  lazyMatrix lm(nrow, ncol);
  for(size_t j = 0; j < ncol; j++) {
    Rcpp::NumericVector colj = nm(Rcpp::_, j);
    for(size_t i = 0; i < nrow; i++) {
      if(isinf(colj(i))) {
        if(colj(i) > 0) {
          lazyNumber x(Quotient(1) / Quotient(0));
          lm(i, j) = lazyScalar(x);
        } else {
          lazyNumber x(Quotient(-1) / Quotient(0));
          lm(i, j) = lazyScalar(x);
        }
      } else if(isNaN(colj, i)) {
        lazyNumber x(Quotient(0) / Quotient(0));
        lm(i, j) = lazyScalar(x);
      } else if(Rcpp::NumericVector::is_na(colj(i))) {
        lm(i, j) = std::nullopt;
      } else {
        lm(i, j) = lazyScalar(nm(i, j));
      }
    }
  }
  return lazyMatrixXPtr(new lazyMatrix(lm), false);
}

// [[Rcpp::export]]
Rcpp::NumericVector lvx2nv(lazyVectorXPtr lvx) {
  lazyVector lv = *(lvx.get());
  const size_t n = lv.size();
  Rcpp::NumericVector nv(n);
  for(size_t i = 0; i < n; i++) {
    lazyScalar x = lv[i];
    if(x) {
      nv(i) = CGAL::to_double<Quotient>((*x).exact());
    } else {
      nv(i) = Rcpp::NumericVector::get_na();
    }
  }
  return nv;
}

// [[Rcpp::export]]
Rcpp::NumericMatrix lmx2nm(lazyMatrixXPtr lmx) {
  lazyMatrix lm = *(lmx.get());
  const size_t nrow = lm.rows();
  const size_t ncol = lm.cols();
  Rcpp::NumericMatrix nm(nrow, ncol);
  for(size_t i = 0; i < nrow; i++) {
    for(size_t j = 0; j < ncol; j++) {
      lazyScalar x = lm.coeff(i, j);
      if(x) {
        nm(i, j) = CGAL::to_double<Quotient>((*x).exact());
      } else {
        nm(i, j) = Rcpp::NumericVector::get_na();
      }
    }
  }
  return nm;
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
lazyMatrixXPtr minus_lmx(lazyMatrixXPtr lmx) {
  lazyMatrix lm = *(lmx.get());
  return lazyMatrixXPtr(new lazyMatrix(-lm), false);
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
    lazyScalar ln1 = lv1[0];
    for(size_t i = 0; i < n2; i++) {
      lv.emplace_back(ln1 + lv2[i]);
    }
  } else if(n2 == 1) {
    lv.reserve(n1);
    lazyScalar ln2 = lv2[0];
    for(size_t i = 0; i < n1; i++) {
      lv.emplace_back(lv1[i] + ln2);
    }
  } else {
    Rcpp::stop("Incompatible lengths.");
  }
  return lazyVectorXPtr(new lazyVector(lv), false);
}

// [[Rcpp::export]]
lazyMatrixXPtr lmx_plus_lmx(lazyMatrixXPtr lmx1, lazyMatrixXPtr lmx2) {
  lazyMatrix lm1 = *(lmx1.get());
  lazyMatrix lm2 = *(lmx2.get());
  return lazyMatrixXPtr(new lazyMatrix(lm1 + lm2), false);
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
    lazyScalar ln1 = lv1[0];
    for(size_t i = 0; i < n2; i++) {
      lv.emplace_back(ln1 - lv2[i]);
    }
  } else if(n2 == 1) {
    lv.reserve(n1);
    lazyScalar ln2 = lv2[0];
    for(size_t i = 0; i < n1; i++) {
      lv.emplace_back(lv1[i] - ln2);
    }
  } else {
    Rcpp::stop("Incompatible lengths.");
  }
  return lazyVectorXPtr(new lazyVector(lv), false);
}

// [[Rcpp::export]]
lazyMatrixXPtr lmx_minus_lmx(lazyMatrixXPtr lmx1, lazyMatrixXPtr lmx2) {
  lazyMatrix lm1 = *(lmx1.get());
  lazyMatrix lm2 = *(lmx2.get());
  return lazyMatrixXPtr(new lazyMatrix(lm1 - lm2), false);
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
    lazyScalar ln1 = lv1[0];
    for(size_t i = 0; i < n2; i++) {
      lv.emplace_back(ln1 * lv2[i]);
    }
  } else if(n2 == 1) {
    lv.reserve(n1);
    lazyScalar ln2 = lv2[0];
    for(size_t i = 0; i < n1; i++) {
      lv.emplace_back(lv1[i] * ln2);
    }
  } else {
    Rcpp::stop("Incompatible lengths.");
  }
  return lazyVectorXPtr(new lazyVector(lv), false);
}

// [[Rcpp::export]]
lazyMatrixXPtr lmx_cwtimes_lmx(lazyMatrixXPtr lmx1, lazyMatrixXPtr lmx2) {
  lazyMatrix lm1 = *(lmx1.get());
  lazyMatrix lm2 = *(lmx2.get());
  return lazyMatrixXPtr(new lazyMatrix(lm1.cwiseProduct(lm2)), false);
}

// [[Rcpp::export]]
lazyMatrixXPtr lmx_times_lmx(lazyMatrixXPtr lmx1, lazyMatrixXPtr lmx2) {
  lazyMatrix lm1 = *(lmx1.get());
  lazyMatrix lm2 = *(lmx2.get());
  return lazyMatrixXPtr(new lazyMatrix(lm1 * lm2), false);
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
      lv.emplace_back(lv1[i] / lv2[i]);
    }
  } else if(n1 == 1) {
    lv.reserve(n2);
    lazyScalar ls1 = lv1[0];
    for(size_t i = 0; i < n2; i++) {
      lv.emplace_back(ls1 / lv2[i]);
    }
  } else if(n2 == 1) {
    lazyScalar ls2 = lv2[0];
    for(size_t i = 0; i < n1; i++) {
      lv.emplace_back(lv1[i] / ls2);
    }
  } else {
    Rcpp::stop("Incompatible lengths.");
  }
  return lazyVectorXPtr(new lazyVector(lv), false);
}

// [[Rcpp::export]]
lazyMatrixXPtr lmx_dividedby_lmx(lazyMatrixXPtr lmx1, lazyMatrixXPtr lmx2) {
  lazyMatrix lm1 = *(lmx1.get());
  lazyMatrix lm2 = *(lmx2.get());
  const size_t nrow = lm1.rows();
  const size_t ncol = lm1.cols();
  lazyMatrix lm(nrow, ncol);
  for(size_t i = 0; i < nrow; i++) {
    for(size_t j = 0; j < ncol; j++) {
      lm(i, j) = lm1.coeff(i, j) / lm2.coeff(i, j);
    }
  }
  return lazyMatrixXPtr(new lazyMatrix(lm), false);
}

lazyVectorXPtr lazySum0(lazyVector lv, bool na_rm) {
  lazyNumber sum0(0);
  if(na_rm) {
    for(size_t i = 0; i < lv.size(); i++) {
      lazyScalar x = lv[i];
      if(x) {
        sum0 += *x;
      }
    }
  } else {
    for(size_t i = 0; i < lv.size(); i++) {
      lazyScalar x = lv[i];
      if(x) {
        sum0 += *x;
      } else {
        return lazyVectorXPtr(new lazyVector({std::nullopt}), false);
      }
    }
  }
  return lazyVectorXPtr(new lazyVector({sum0}), false);
}

// [[Rcpp::export]]
lazyVectorXPtr lazySum(lazyVectorXPtr lvx, bool na_rm) {
  return lazySum0(*(lvx.get()), na_rm);
}

lazyVectorXPtr lazyProd0(lazyVector lv, bool na_rm) {
  lazyNumber prod0(1);
  if(na_rm) {
    for(size_t i = 0; i < lv.size(); i++) {
      lazyScalar x = lv[i];
      if(x) {
        prod0 *= *x;
      }
    }
  } else {
    for(size_t i = 0; i < lv.size(); i++) {
      lazyScalar x = lv[i];
      if(x) {
        prod0 *= *x;
      } else {
        return lazyVectorXPtr(new lazyVector({std::nullopt}), false);
      }
    }
  }
  return lazyVectorXPtr(new lazyVector({prod0}), false);
}

// [[Rcpp::export]]
lazyVectorXPtr lazyProd(lazyVectorXPtr lvx, bool na_rm) {
  return lazyProd0(*(lvx.get()), na_rm);
}

// [[Rcpp::export]]
lazyVectorXPtr lazyCumsum(lazyVectorXPtr lvx) {
  lazyVector lvin = *(lvx.get());
  const size_t n = lvin.size();
  lazyVector lv(n);
  lazyScalar sum(0);
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
  lazyScalar prod(1);
  for(size_t i = 0; i < n; i++) {
    prod *= lvin[i];
    lv[i] = prod;
  }
  return lazyVectorXPtr(new lazyVector(lv), false);
}

// [[Rcpp::export]]
lazyVectorXPtr lazyConcat(
    lazyVectorXPtr lvx1, lazyVectorXPtr lvx2
) {
  lazyVector lv = *(lvx1.get());
  lazyVector lv2 = *(lvx2.get());
  std::copy(lv2.begin(), lv2.end(), std::back_inserter(lv));
  return lazyVectorXPtr(new lazyVector(lv), false);
}

// // [[Rcpp::export]]
// lazyVectorXPtr lazyDuplicate(lazyVectorXPtr lvx) {
//   lazyVector lvin = *(lvx.get());
//   lazyVector lv;
//   std::copy(lvin.begin(), lvin.end(), std::back_inserter(lv));
//   return lazyVectorXPtr(new lazyVector(lv), false);
// }

bool compareLazyScalars(lazyScalar x, lazyScalar y) {
  return *x < *y;
}

lazyVectorXPtr lazyMax0(lazyVector lv, bool na_rm) {
  lazyScalar max;
  if(na_rm) {
    lazyVector lv2 = lazyNAomit0(lv);
    if(lv2.size() == 0) {
      max = -lazyScalar(1) / lazyScalar(0);
    } else {
      lazyVector::iterator result;
      result = std::max_element(lv2.begin(), lv2.end(), compareLazyScalars);
      max = *result;
    }
  } else {
    const size_t n = lv.size();
    max = lv[0];
    if(!max) {
      return lazyVectorXPtr(new lazyVector({std::nullopt}), false);
    }
    for(size_t i = 1; i < n; i++) {
      lazyScalar candidate = lv[i];
      if(!candidate) {
        return lazyVectorXPtr(new lazyVector({std::nullopt}), false);
      }
      if(*candidate > *max) {
        max = candidate; 
      }
    }
  }
  return lazyVectorXPtr(new lazyVector({max}), false);
}

// [[Rcpp::export]]
lazyVectorXPtr lazyMax(lazyVectorXPtr lvx, bool na_rm) {
  return lazyMax0(*(lvx.get()), na_rm); 
}

lazyVectorXPtr lazyMin0(lazyVector lv, bool na_rm) {
  lazyScalar min;
  if(na_rm) {
    lazyVector lv2 = lazyNAomit0(lv);
    if(lv2.size() == 0) {
      min = lazyScalar(1) / lazyScalar(0);
    } else {
      lazyVector::iterator result;
      result = std::min_element(lv2.begin(), lv2.end(), compareLazyScalars);
      min = *result;
    }
  } else {
    min = lv[0];
    if(!min) {
      return lazyVectorXPtr(new lazyVector({std::nullopt}), false);
    }
    for(size_t i = 1; i < lv.size(); i++) {
      lazyScalar candidate = lv[i];
      if(!candidate) {
        return lazyVectorXPtr(new lazyVector({std::nullopt}), false);
      }
      if(*candidate < *min) {
        min = candidate; 
      }
    }
  }
  return lazyVectorXPtr(new lazyVector({min}), false);
}

// [[Rcpp::export]]
lazyVectorXPtr lazyMin(lazyVectorXPtr lvx, bool na_rm) {
  return lazyMin0(*(lvx.get()), na_rm); 
}

// [[Rcpp::export]]
lazyVectorXPtr lazyRange(lazyVectorXPtr lvx, bool na_rm) {
  lazyVectorXPtr min = lazyMin(lvx, na_rm);
  lazyVectorXPtr max = lazyMax(lvx, na_rm);
  return lazyConcat(min, max);
}

// [[Rcpp::export]]
lazyVectorXPtr MlazyProd(lazyMatrixXPtr lmx, bool na_rm) {
  lazyMatrix lm = *(lmx.get());
  lazyVector lv(lm.data(), lm.data() + lm.size());
  return lazyProd0(lv, na_rm);
}

// [[Rcpp::export]]
lazyVectorXPtr MlazySum(lazyMatrixXPtr lmx, bool na_rm) {
  lazyMatrix lm = *(lmx.get());
  lazyVector lv(lm.data(), lm.data() + lm.size());
  return lazySum0(lv, na_rm);
}

// [[Rcpp::export]]
lazyVectorXPtr MlazyMax(lazyMatrixXPtr lmx, bool na_rm) {
  lazyMatrix lm = *(lmx.get());
  lazyVector lv(lm.data(), lm.data() + lm.size());
  return lazyMax0(lv, na_rm);
}

// [[Rcpp::export]]
lazyVectorXPtr MlazyMin(lazyMatrixXPtr lmx, bool na_rm) {
  lazyMatrix lm = *(lmx.get());
  lazyVector lv(lm.data(), lm.data() + lm.size());
  return lazyMin0(lv, na_rm);
}

// [[Rcpp::export]]
lazyVectorXPtr MlazyRange(lazyMatrixXPtr lmx, bool na_rm) {
  return lazyConcat(MlazyMin(lmx, na_rm), MlazyMax(lmx, na_rm));
}

lazyScalar lazyScalarPower(lazyScalar x, int alpha) {
  if(!x) {
    return std::nullopt;
  }
  if(alpha < 0) {
    lazyScalar invx(lazyNumber(1) / x);
    return lazyScalarPower(invx, -alpha);
  }
  lazyScalar result(1);
  while(alpha) {
    if(alpha & 1) {
      result *= x;
    }
    alpha >>= 1;
    x *= x;
  }
  return result;
}

// [[Rcpp::export]]
lazyVectorXPtr lazyPower(lazyVectorXPtr lvx, int alpha) {
  lazyVector lvin = *(lvx.get());
  size_t n = lvin.size();
  lazyVector lv(n);
  for(size_t i = 0; i < n; i++) {
    lv[i] = lazyScalarPower(lvin[i], alpha);
  }
  return lazyVectorXPtr(new lazyVector(lv), false);
}

// [[Rcpp::export]]
lazyMatrixXPtr MlazyPower(lazyMatrixXPtr lmx, int alpha) {
  lazyMatrix lmin = *(lmx.get());
  size_t m = lmin.rows();
  size_t n = lmin.cols();
  lazyMatrix lm(m, n);
  for(size_t i = 0; i < m; i++) {
    for(size_t j = 0; j < n; j++) {
      lm(i, j) = lazyScalarPower(lmin.coeff(i, j), alpha);
    }
  }
  return lazyMatrixXPtr(new lazyMatrix(lm), false);
}

// [[Rcpp::export]]
lazyVectorXPtr lazyAbs(lazyVectorXPtr lvx) {
  lazyVector lvin = *(lvx.get());
  size_t n = lvin.size();
  lazyVector lv(n);
  for(size_t i = 0; i < n; i++) {
    lazyScalar x = lvin[i];
    if(x) {
      lv[i] = CGAL::abs(*x);  
    } else {
      lv[i] = std::nullopt;
    }
  }
  return lazyVectorXPtr(new lazyVector(lv), false);
}

// [[Rcpp::export]]
lazyMatrixXPtr MlazyAbs(lazyMatrixXPtr lmx) {
  lazyMatrix lmin = *(lmx.get());
  size_t m = lmin.rows();
  size_t n = lmin.cols();
  lazyMatrix lm(m, n);
  for(size_t i = 0; i < m; i++) {
    for(size_t j = 0; j < n; j++) {
      lazyScalar x = lmin.coeff(i, j);
      if(x) {
        lm(i, j) = CGAL::abs(*x);
      } else {
        lm(i, j) = std::nullopt;
      }
    }
  }
  return lazyMatrixXPtr(new lazyMatrix(lm), false);
}

// [[Rcpp::export]]
lazyVectorXPtr lazyExtract(lazyVectorXPtr lvx, Rcpp::IntegerVector indices) {
  lazyVector lvin = *(lvx.get());
  size_t n = indices.size();
  lazyVector lv(n);
  for(size_t i = 0; i < n; i++) {
    lv[i] = lvin[indices(i)];
  }
  return lazyVectorXPtr(new lazyVector(lv), false);
}

// [[Rcpp::export]]
lazyMatrixXPtr MlazyExtract(
    lazyMatrixXPtr lmx, Rcpp::IntegerMatrix indices, int m, int n
) {
  lazyMatrix lmin = *(lmx.get());
  Rcpp::IntegerVector is = indices(Rcpp::_, 0);
  Rcpp::IntegerVector js = indices(Rcpp::_, 1);
  lazyMatrix lm(m, n);
  size_t k = 0;
  for(int j = 0; j < n; j++) {
    for(int i = 0; i < m; i++){
      lm(i, j) = lmin.coeff(is(k), js(k));
      k++;
    }
  }
  return lazyMatrixXPtr(new lazyMatrix(lm), false);
}

// [[Rcpp::export]]
lazyVectorXPtr lazyReplace(
    lazyVectorXPtr lvx1, Rcpp::IntegerVector indices, lazyVectorXPtr lvx2
) {
  lazyVector lv = *(lvx1.get());
  lazyVector lv2 = *(lvx2.get());
  size_t n = indices.size();
  for(size_t i = 0; i < n; i++) {
    lv[indices(i)] = lv2[i];
  }
  return lazyVectorXPtr(new lazyVector(lv), false);
}

// [[Rcpp::export]]
lazyMatrixXPtr lazyColumnMatrix(lazyVectorXPtr lvx) {
  lazyVector lv = *(lvx.get());
  size_t n = lv.size();
  lazyMatrix lm = Eigen::Map<lazyMatrix>(lv.data(), n, 1);
  return lazyMatrixXPtr(new lazyMatrix(lm), false);
  // lazyMatrix lm(n, 1);
  // for(size_t i = 0; i < n; i++) {
  //   lm(i, 0) = lv[i];
  // }
  // return lazyMatrixXPtr(new lazyMatrix(lm), false);
}

// [[Rcpp::export]]
lazyMatrixXPtr lazyRowMatrix(lazyVectorXPtr lvx) {
  lazyVector lv = *(lvx.get());
  size_t n = lv.size();
  lazyMatrix lm = Eigen::Map<lazyMatrix>(lv.data(), 1, n);
  return lazyMatrixXPtr(new lazyMatrix(lm), false);
  // lazyMatrix lm(1, n);
  // for(size_t i = 0; i < n; i++) {
  //   lm(0, i) = lv[i];
  // }
  // return lazyMatrixXPtr(new lazyMatrix(lm), false);
}

// [[Rcpp::export]]
lazyMatrixXPtr lazyRbind(
    lazyMatrixXPtr lmx1, lazyMatrixXPtr lmx2
) {
  lazyMatrix lm = *(lmx1.get());
  lazyMatrix lm2 = *(lmx2.get());
  lm.conservativeResize(lm.rows() + lm2.rows(), Eigen::NoChange);
  lm.bottomRows(lm2.rows()) = lm2;
  return lazyMatrixXPtr(new lazyMatrix(lm), false);
}

// [[Rcpp::export]]
lazyMatrixXPtr lazyCbind(
    lazyMatrixXPtr lmx1, lazyMatrixXPtr lmx2
) {
  lazyMatrix lm = *(lmx1.get());
  lazyMatrix lm2 = *(lmx2.get());
  lm.conservativeResize(Eigen::NoChange, lm.cols() + lm2.cols());
  lm.rightCols(lm2.cols()) = lm2;
  return lazyMatrixXPtr(new lazyMatrix(lm), false);
}

// [[Rcpp::export]]
lazyVectorXPtr lazyDeterminant(lazyMatrixXPtr lmx) {
  lazyMatrix lm = *(lmx.get());
  const size_t m = lm.rows();
  const size_t n = lm.cols();
  lazyMatrix0 lm0(m, n);
  for(size_t i = 0; i < m; i++) {
    for(size_t j = 0; j < n; j++) {
      lazyScalar x = lm.coeff(i, j);
      if(x) {
        lm0(i, j) = *x;
      } else {
        return lazyVectorXPtr(new lazyVector({std::nullopt}), false);
      }
    }
  }
  return lazyVectorXPtr(new lazyVector({lm0.determinant()}), false);
}

// [[Rcpp::export]]
lazyMatrixXPtr lazyInverse(lazyMatrixXPtr lmx) {
  lazyMatrix lm = *(lmx.get());
  return lazyMatrixXPtr(new lazyMatrix(lm.inverse()), false);
}

// [[Rcpp::export]]
lazyMatrixXPtr lazyTranspose(lazyMatrixXPtr lmx) {
  lazyMatrix lm = *(lmx.get());
  return lazyMatrixXPtr(new lazyMatrix(lm.transpose()), false);
}

// [[Rcpp::export]]
lazyVectorXPtr lazyFlatten(lazyMatrixXPtr lmx) {
  lazyMatrix lm = *(lmx.get());
  lazyVector lv(lm.data(), lm.data() + lm.size());
  // lazyVector lv;
  // const size_t s = lm.size();
  // lv.reserve(s);
  // for(size_t k = 0; k < s; k++) {
  //   lv.emplace_back(*(lm.data() + k));
  // }
  // size_t m = lm.rows();
  // size_t n = lm.cols();
  // lv.reserve(m * n);
  // for(size_t j = 0; j < n; j++) {
  //   for(size_t i = 0; i < m; i++){
  //     lv.emplace_back(lm.coeff(i, j));
  //   }
  // }
  return lazyVectorXPtr(new lazyVector(lv), false);
}

// [[Rcpp::export]]
Rcpp::LogicalVector lazyCompare(
    lazyVectorXPtr lvx1, lazyVectorXPtr lvx2, Rcpp::String r
) {
  lazyVector lv1 = *(lvx1.get());
  lazyVector lv2 = *(lvx2.get());
  size_t n;
  const size_t n1 = lv1.size();
  const size_t n2 = lv2.size();
  if(n1 != n2) {
    if(n1 == 1) {
      lv1 = lazyVector(n2, lv1[0]);
      n = n2;
    } else {
      lv2 = lazyVector(n1, lv2[0]);
      n = n1;
    }
  } else {
    n = n1;
  }
  Rcpp::LogicalVector out(n);
  if(r == "==") {
    for(size_t i = 0; i < n; i++) {
      lazyScalar x = lv1[i];
      lazyScalar y = lv2[i];
      if(x && y) {
        out(i) = *x == *y;
      } else {
        out(i) = Rcpp::LogicalVector::get_na();
      }
    }
  } else if(r == "!=") {
    for(size_t i = 0; i < n; i++) {
      lazyScalar x = lv1[i];
      lazyScalar y = lv2[i];
      if(x && y) {
        out(i) = *x != *y;
      } else {
        out(i) = Rcpp::LogicalVector::get_na();
      }
    }
  } else if(r == "<") {
    for(size_t i = 0; i < n; i++) {
      lazyScalar x = lv1[i];
      lazyScalar y = lv2[i];
      if(x && y) {
        out(i) = *x < *y;
      } else {
        out(i) = Rcpp::LogicalVector::get_na();
      }
    }
  } else if(r == "<=") {
    for(size_t i = 0; i < n; i++) {
      lazyScalar x = lv1[i];
      lazyScalar y = lv2[i];
      if(x && y) {
        out(i) = *x <= *y;
      } else {
        out(i) = Rcpp::LogicalVector::get_na();
      }
    }
  } else if(r == ">") {
    for(size_t i = 0; i < n; i++) {
      lazyScalar x = lv1[i];
      lazyScalar y = lv2[i];
      if(x && y) {
        out(i) = *x > *y;
      } else {
        out(i) = Rcpp::LogicalVector::get_na();
      }
    }
  } else {
    for(size_t i = 0; i < n; i++) {
      lazyScalar x = lv1[i];
      lazyScalar y = lv2[i];
      if(x && y) {
        out(i) = *x >= *y;
      } else {
        out(i) = Rcpp::LogicalVector::get_na();
      }
    }
  }
  return out;
}

// [[Rcpp::export]]
lazyVectorXPtr lazyDiagonal(lazyMatrixXPtr lmx) {
  lazyMatrix lm = *(lmx.get());
  const size_t n = lm.cols();
  lazyVector lv;
  lv.reserve(n);
  for(size_t k = 0; k < lm.size(); k += n+1) {
    lv.emplace_back(*(lm.data() + k));
  }
  return lazyVectorXPtr(new lazyVector(lv), false);
}

// [[Rcpp::export]]
lazyMatrixXPtr lazyReplaceDiagonal(lazyMatrixXPtr lmx, lazyVectorXPtr lvx) {
  lazyMatrix lm = *(lmx.get());
  lazyVector lv = *(lvx.get());
  const size_t n = lv.size();
  for(size_t k = 0; k < n; k++) {
    lm(k, k) = lv[k];
  }
  return lazyMatrixXPtr(new lazyMatrix(lm), false);
}
