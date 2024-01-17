#include <sycl/sycl.hpp>

/*@ 
  requires \pointer(a, 10, write);
*/
void test(bool* a) {
  sycl::buffer<bool, 1> aBuffer = sycl::buffer(a, sycl::range<1>(11)); // Buffer to big, so should fail
}