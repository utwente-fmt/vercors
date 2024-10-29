#include <sycl/sycl.hpp>

//@ requires \pointer(a, 10, write);
void test(bool* a) {
  sycl::buffer<bool, 1> aBuffer = sycl::buffer<bool, 1>(a, sycl::range<1>(9));
  a[9] = true;  // Should be allowed as buffer only claimed indices 0-8
}