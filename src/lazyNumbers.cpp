#include "lazyNumbers_types.h"

// [[Rcpp::export]]
void lazyExact(lazyVectorXPtr lvx) {
  lazyVector lv = *(lvx.get());
  for(size_t i = 0; i < lv.size(); i++) {
    Quotient q = lv[i].exact();
  }
}

// [[Rcpp::export]]
void MlazyExact(lazyMatrixXPtr lmx) {
  lazyMatrix lm = *(lmx.get());
  for(size_t i = 0; i < lm.rows(); i++) {
    for(size_t j = 0; j < lm.cols(); j++) {
      Quotient q = lm.coeff(i, j).exact();
    }
  }
}

// [[Rcpp::export]]
Rcpp::List intervals_lvx(lazyVectorXPtr lvx) {
  lazyVector lv = *(lvx.get());
  const size_t n = lv.size();
  Rcpp::NumericVector inf(n);
  Rcpp::NumericVector sup(n);
  for(size_t i = 0; i < n; i++) {
    CGAL::Interval_nt<false> interval = lv[i].approx();
    inf(i) = interval.inf();
    sup(i) = interval.sup();
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
      CGAL::Interval_nt<false> interval = lm.coeff(i,j).approx();
      inf(i, j) = interval.inf();
      sup(i, j) = interval.sup();
    }
  }
  return Rcpp::List::create(Rcpp::Named("inf") = inf, Rcpp::Named("sup") = sup);
}

// [[Rcpp::export]]
lazyVectorXPtr nv2lvx(Rcpp::NumericVector nv) {
  const size_t n = nv.size();
  lazyVector lv(n);
  for(size_t i = 0; i < n; i++) {
    lv[i] = lazyScalar(nv(i));
  }
  return lazyVectorXPtr(new lazyVector(lv), false);
}

// [[Rcpp::export]]
lazyMatrixXPtr nm2lmx(Rcpp::NumericMatrix nm) {
  const size_t nrow = nm.nrow();
  const size_t ncol = nm.ncol();
  lazyMatrix lm(nrow, ncol);
  for(size_t i = 0; i < nrow; i++) {
    for(size_t j = 0; j < ncol; j++) {
      lm(i, j) = lazyScalar(nm(i, j));
    }
  }
  return lazyMatrixXPtr(new lazyMatrix(lm), false);
}

// [[Rcpp::export]]
Rcpp::NumericVector lvx2nv(lazyVectorXPtr lvx, double prec) {
  lazyVector lv = *(lvx.get());
  const size_t n = lv.size();
  Rcpp::NumericVector nv(n);
  lazyScalar::set_relative_precision_of_to_double(prec);
  for(size_t i = 0; i < n; i++) {
    nv(i) = CGAL::to_double<Quotient>(lv[i].exact());
  }
  return nv;
}

// [[Rcpp::export]]
Rcpp::NumericMatrix lmx2nm(lazyMatrixXPtr lmx, double prec) {
  lazyMatrix lm = *(lmx.get());
  const size_t nrow = lm.rows();
  const size_t ncol = lm.cols();
  Rcpp::NumericMatrix nm(nrow, ncol);
  lazyScalar::set_relative_precision_of_to_double(prec);
  for(size_t i = 0; i < nrow; i++) {
    for(size_t j = 0; j < ncol; j++) {
      nm(i, j) = CGAL::to_double<Quotient>(lm.coeff(i, j).exact());
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
  lazyMatrix lmin = *(lmx.get());
  lazyMatrix lm = - lmin;
  return lazyMatrixXPtr(new lazyMatrix(lm), false);
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
  lazyMatrix lm = lm1 + lm2;
  return lazyMatrixXPtr(new lazyMatrix(lm), false);
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
  lazyMatrix lm = lm1 - lm2;
  return lazyMatrixXPtr(new lazyMatrix(lm), false);
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
  lazyMatrix lm = lm1.cwiseProduct(lm2);
  return lazyMatrixXPtr(new lazyMatrix(lm), false);
}

// [[Rcpp::export]]
lazyMatrixXPtr lmx_times_lmx(lazyMatrixXPtr lmx1, lazyMatrixXPtr lmx2) {
  lazyMatrix lm1 = *(lmx1.get());
  lazyMatrix lm2 = *(lmx2.get());
  lazyMatrix lm = lm1 * lm2;
  return lazyMatrixXPtr(new lazyMatrix(lm), false);
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
      lazyScalar ls2 = lv2[i];
      if(CGAL::is_zero(ls2)) {
        Rcpp::stop("Division by zero.");
      }
      lv.emplace_back(lv1[i] / ls2);
    }
  } else if(n1 == 1) {
    lv.reserve(n2);
    lazyScalar ls1 = lv1[0];
    for(size_t i = 0; i < n2; i++) {
      lazyScalar ls2 = lv2[i];
      if(CGAL::is_zero(ls2)) {
        Rcpp::stop("Division by zero.");
      }
      lv.emplace_back(ls1 / ls2);
    }
  } else if(n2 == 1) {
    lazyScalar ls2 = lv2[0];
    if(CGAL::is_zero(ls2)) {
      Rcpp::stop("Division by zero.");
    }
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
      lazyScalar ls2 = lm2.coeff(i, j);
      if(CGAL::is_zero(ls2)) {
        Rcpp::stop("Division by zero.");
      }
      lm(i, j) = lm1.coeff(i, j) / ls2;
    }
  }
  return lazyMatrixXPtr(new lazyMatrix(lm), false);
}

// [[Rcpp::export]]
lazyVectorXPtr lazySum(lazyVectorXPtr lvx) {
  lazyVector lvin = *(lvx.get());
  const size_t n = lvin.size();
  lazyScalar sum(0);
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
  lazyScalar prod(1);
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
lazyVectorXPtr lazyMax(lazyVectorXPtr lvx) {
  lazyVector lvin = *(lvx.get());
  const size_t n = lvin.size();
  lazyScalar max(lvin[0]);
  for(size_t i = 1; i < n; i++) {
    lazyScalar candidate = lvin[i];
    if(candidate > max) {
      max = candidate; 
    }
  }
  lazyVector lv = {max};
  return lazyVectorXPtr(new lazyVector(lv), false);
}

// [[Rcpp::export]]
lazyVectorXPtr lazyMin(lazyVectorXPtr lvx) {
  lazyVector lvin = *(lvx.get());
  const size_t n = lvin.size();
  lazyScalar min(lvin[0]);
  for(size_t i = 1; i < n; i++) {
    lazyScalar candidate = lvin[i];
    if(candidate < min) {
      min = candidate; 
    }
  }
  lazyVector lv = {min};
  return lazyVectorXPtr(new lazyVector(lv), false);
}

// [[Rcpp::export]]
lazyVectorXPtr lazyRange(lazyVectorXPtr lvx) {
  lazyVector lvin = *(lvx.get());
  const size_t n = lvin.size();
  lazyScalar min(lvin[0]);
  lazyScalar max(lvin[0]);
  for(size_t i = 1; i < n; i++) {
    lazyScalar candidate = lvin[i];
    if(candidate < min) {
      min = candidate; 
    }
    if(candidate > max) {
      max = candidate; 
    }
  }
  lazyVector lv = {min, max};
  return lazyVectorXPtr(new lazyVector(lv), false);
}

// [[Rcpp::export]]
lazyVectorXPtr MlazyProd(lazyMatrixXPtr lmx) {
  lazyMatrix lm = *(lmx.get());
  lazyScalar prod = lm.prod();
  lazyVector lv = {prod};
  return lazyVectorXPtr(new lazyVector(lv), false);
}

// [[Rcpp::export]]
lazyVectorXPtr MlazySum(lazyMatrixXPtr lmx) {
  lazyMatrix lm = *(lmx.get());
  lazyVector lv = {lm.sum()};
  return lazyVectorXPtr(new lazyVector(lv), false);
}

// [[Rcpp::export]]
lazyVectorXPtr MlazyMax(lazyMatrixXPtr lmx) {
  lazyMatrix lm = *(lmx.get());
  lazyVector lv = {lm.maxCoeff()};
  return lazyVectorXPtr(new lazyVector(lv), false);
}

// [[Rcpp::export]]
lazyVectorXPtr MlazyMin(lazyMatrixXPtr lmx) {
  lazyMatrix lm = *(lmx.get());
  lazyVector lv = {lm.minCoeff()};
  return lazyVectorXPtr(new lazyVector(lv), false);
}

// [[Rcpp::export]]
lazyVectorXPtr MlazyRange(lazyMatrixXPtr lmx) {
  lazyMatrix lm = *(lmx.get());
  lazyVector lv = {lm.minCoeff(), lm.maxCoeff()};
  return lazyVectorXPtr(new lazyVector(lv), false);
}

lazyScalar lazyScalarPower(lazyScalar x, int alpha) {
  if(alpha < 0) {
    Quotient q = x.exact();
    lazyScalar invx(Quotient(q.denominator(), q.numerator()));
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
    lv[i] = CGAL::abs(lvin[i]);
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
      lm(i, j) = CGAL::abs(lmin.coeff(i, j));
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
lazyVectorXPtr lazyConcat(
    lazyVectorXPtr lvx1, lazyVectorXPtr lvx2
) {
  lazyVector lv = *(lvx1.get());
  lazyVector lv2 = *(lvx2.get());
  std::copy(lv2.begin(), lv2.end(), std::back_inserter(lv));
  return lazyVectorXPtr(new lazyVector(lv), false);
}

// [[Rcpp::export]]
lazyMatrixXPtr lazyColumnMatrix(lazyVectorXPtr lvx) {
  lazyVector lv = *(lvx.get());
  size_t n = lv.size();
  lazyMatrix lm(n, 1);
  for(size_t i = 0; i < n; i++) {
    lm(i, 0) = lv[i];
  }
  return lazyMatrixXPtr(new lazyMatrix(lm), false);
}

// [[Rcpp::export]]
lazyMatrixXPtr lazyRowMatrix(lazyVectorXPtr lvx) {
  lazyVector lv = *(lvx.get());
  size_t n = lv.size();
  lazyMatrix lm(1, n);
  for(size_t i = 0; i < n; i++) {
    lm(0, i) = lv[i];
  }
  return lazyMatrixXPtr(new lazyMatrix(lm), false);
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
  lazyVector det = {lm.determinant()};
  return lazyVectorXPtr(new lazyVector(det), false);
}

// [[Rcpp::export]]
lazyMatrixXPtr lazyInverse(lazyMatrixXPtr lmx) {
  lazyMatrix lm = *(lmx.get());
  lazyMatrix inv = lm.inverse();
  return lazyMatrixXPtr(new lazyMatrix(inv), false);
}

// [[Rcpp::export]]
lazyMatrixXPtr lazyTranspose(lazyMatrixXPtr lmx) {
  lazyMatrix lm = *(lmx.get());
  return lazyMatrixXPtr(new lazyMatrix(lm.transpose()), false);
}

// [[Rcpp::export]]
lazyVectorXPtr lazyFlatten(lazyMatrixXPtr lmx) {
  lazyMatrix lm = *(lmx.get());
  lazyVector lv;
  const size_t s = lm.size();
  lv.reserve(s);
  for(size_t k = 0; k < s; k++) {
    lv.emplace_back(*(lm.data() + k));
  }
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
      out(i) = lv1[i] == lv2[i];
    }
  } else if(r == "!=") {
    for(size_t i = 0; i < n; i++) {
      out(i) = lv1[i] != lv2[i];
    }
  } else if(r == "<") {
    for(size_t i = 0; i < n; i++) {
      out(i) = lv1[i] < lv2[i];
    }
  } else if(r == "<=") {
    for(size_t i = 0; i < n; i++) {
      out(i) = lv1[i] <= lv2[i];
    }
  } else if(r == ">") {
    for(size_t i = 0; i < n; i++) {
      out(i) = lv1[i] > lv2[i];
    }
  } else {
    for(size_t i = 0; i < n; i++) {
      out(i) = lv1[i] >= lv2[i];
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
