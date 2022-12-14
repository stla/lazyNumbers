#include <vector>
#include <CGAL/number_utils.h>
#include <CGAL/Lazy_exact_nt.h>
#include <CGAL/MP_Float.h>
#include <CGAL/Quotient.h>
#include <CGAL/Interval_nt.h>

typedef CGAL::Quotient<CGAL::MP_Float> Quotient;
typedef CGAL::Lazy_exact_nt<Quotient>  lazyScalar;
typedef std::vector<lazyScalar>        lazyVector;


lazyScalar mydivision(lazyScalar x1, lazyScalar x2) {
  Quotient q1 = x1.exact();
  Quotient q2 = x2.exact();
  lazyScalar q = lazyScalar(Quotient(
    q1.numerator() * q2.denominator(), q1.denominator() * q2.numerator())
  );
  return q;
}

lazyScalar lazySum(lazyVector lv) {
  const size_t n = lv.size();
  lazyScalar sum(0);
  for(size_t i = 0; i < n; i++) {
    sum += lv[i];
  }
  return sum;
}

lazyVector lv1_dividedby_lv2(lazyVector lv1, lazyVector lv2) {
  const size_t n = lv1.size();
  lazyVector lv(n);
  for(size_t i = 0; i < n; i++) {
    lv[i] = lv1[i] / lv2[i];
  }
  return lv;
}

lazyVector lv1_mydividedby_lv2(lazyVector lv1, lazyVector lv2) {
  const size_t n = lv1.size();
  lazyVector lv(n);
  for(size_t i = 0; i < n; i++) {
    lv[i] = mydivision(lv1[i], lv2[i]);
  }
  return lv;
}

lazyVector lazyCumprod(lazyVector lvin) {
  const size_t n = lvin.size();
  lazyVector lv(n);
  lazyScalar prod(1);
  for(size_t i = 0; i < n; i++) {
    prod *= lvin[i];
    lv[i] = prod;
  }
  return lv;
}

// the series with the ordinary division
lazyScalar Euler(int n) {
  lazyVector lv1(n);
  for(int i = 0; i < n; i++) {
    lv1[i] = lazyScalar(i + 1);
  }
  lazyVector lv2(n);
  for(int i = 0; i < n; i++) {
    lv2[i] = lazyScalar(2*i + 3);
  }
  return lazySum(lv1_dividedby_lv2(lazyCumprod(lv1), lazyCumprod(lv2))); 
}

// the series with "my" division
lazyScalar myEuler(int n) {
  lazyVector lv1(n);
  for(int i = 0; i < n; i++) {
    lv1[i] = lazyScalar(i + 1);
  }
  lazyVector lv2(n);
  for(int i = 0; i < n; i++) {
    lv2[i] = lazyScalar(2*i + 3);
  }
  return lazySum(lv1_mydividedby_lv2(lazyCumprod(lv1), lazyCumprod(lv2))); 
}

int main() {
  lazyScalar euler = Euler(171);
  CGAL::Interval_nt<false> interval = euler.approx();
  std::cout << "lower bound: " << interval.inf() << "\n"; // prints 0.57
  std::cout << "upper bound: " << interval.sup() << "\n"; // prints inf
  lazyScalar myeuler = myEuler(171);
  CGAL::Interval_nt<false> myinterval = myeuler.approx();
  std::cout << "lower bound: " << myinterval.inf() << "\n"; // prints 0.57
  std::cout << "upper bound: " << myinterval.sup() << "\n"; // prints 0.57
  return 0;
}