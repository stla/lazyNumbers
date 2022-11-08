#ifndef _HEADER_
#define _HEADER_
#endif

#include <RcppEigen.h>

#define CGAL_EIGEN3_ENABLED 1

#include <CGAL/number_utils.h>
#include <CGAL/Lazy_exact_nt.h>
#include <CGAL/MP_Float.h>
#include <CGAL/Quotient.h>
#include <CGAL/Interval_nt.h>

typedef CGAL::Quotient<CGAL::MP_Float>                            Quotient;
typedef CGAL::Lazy_exact_nt<Quotient>                             lazyScalar;
typedef std::vector<lazyScalar>                                   lazyVector;
typedef Rcpp::XPtr<lazyVector>                                    lazyVectorXPtr;
typedef Eigen::Matrix<lazyScalar, Eigen::Dynamic, Eigen::Dynamic> lazyMatrix;
typedef Rcpp::XPtr<lazyMatrix>                                    lazyMatrixXPtr;
