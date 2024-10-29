#include <sycl/sycl.hpp>

//@ requires \pointer(a, 10, write);
void test(bool a[]) {
  sycl::buffer<bool, 2> aBuffer = sycl::buffer(a, sycl::range<1>(10));
}