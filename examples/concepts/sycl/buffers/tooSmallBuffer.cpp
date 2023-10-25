#include <sycl/sycl.hpp>

/*@ 
  requires \pointer(a, 10, write);
*/
void main(bool* a) {
  sycl::buffer<bool, 1> aBuffer = sycl::buffer<bool, 1>(a, sycl::range<1>(9));
  a[9] = true;  // Should NOT be allowed as buffer claims entire array? // Maybe buffer does not claim entire array but only the specified range?
}