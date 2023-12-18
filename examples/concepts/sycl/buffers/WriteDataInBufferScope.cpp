#include <sycl/sycl.hpp>

/*@ 
  requires \pointer(a, 10, write);
*/
void test(bool* a) {
  sycl::buffer<bool, 1> aBuffer = sycl::buffer(a, sycl::range<1>(10));

  a[5] = true; // Not allowed
}