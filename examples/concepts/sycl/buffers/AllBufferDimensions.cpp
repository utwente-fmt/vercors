#include <sycl/sycl.hpp>

/*@
  requires \pointer(a, 10, write);
  requires \pointer(b, 10, write);
  requires \pointer(c, 10, write);
*/
void test(bool* a, int* b, float* c) { // Does not work when using a[] instead of *a
  sycl::buffer<bool, 1> aBuffer = sycl::buffer(a, sycl::range<1>(10));
  sycl::buffer<int, 2> bBuffer = sycl::buffer(b, sycl::range<2>(2, 5));
  sycl::buffer<float, 3> cBuffer = sycl::buffer(c, sycl::range<3>(2, 5, 1));
}