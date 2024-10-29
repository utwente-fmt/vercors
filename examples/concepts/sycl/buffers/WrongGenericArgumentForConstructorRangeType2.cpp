#include <sycl/sycl.hpp>

//@ requires \pointer(a, 10, write);
void test(bool a[]) {
  // generic arg <int> will be expanded to <int, 1>
  sycl::buffer<int>(a, sycl::range<2>(10));
}