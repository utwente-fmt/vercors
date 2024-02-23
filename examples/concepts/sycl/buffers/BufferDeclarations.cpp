#include <sycl/sycl.hpp>

/*@
  requires \pointer(a, 10, write);
  requires \pointer(b, 10, write);
  requires \pointer(c, 10, write);
*/
void test(bool a[], int* b, float c[]) {
  sycl::buffer<bool, 1> aBuffer = sycl::buffer<bool>(a, sycl::range<1>(10));
  sycl::buffer<int, 2> bBuffer = sycl::buffer<int, 2>(b, sycl::range<2>(2, 5));
  sycl::buffer<float, 3> cBuffer = sycl::buffer(c, sycl::range<3>(2, 5, 1));
}