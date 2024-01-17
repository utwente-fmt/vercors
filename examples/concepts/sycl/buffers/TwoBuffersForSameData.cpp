#include <sycl/sycl.hpp>

/*@ 
  requires \pointer(a, 10, write);
*/
void test(bool* a) {
  sycl::buffer<bool, 1> aBuffer = sycl::buffer(a, sycl::range<1>(10));
  sycl::buffer<bool, 1> aaBuffer = sycl::buffer(a, sycl::range<1>(10)); // Should fail as the previous buffer already claimed indices 0-9
}