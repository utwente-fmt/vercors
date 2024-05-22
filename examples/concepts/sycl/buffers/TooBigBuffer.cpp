#include <sycl/sycl.hpp>

//@ requires \pointer(a, 10, write);
void test(bool* a) {
  // a is only guaranteed to have 10 elements, so cannot make buffer with 11
  sycl::buffer<bool, 1> aBuffer = sycl::buffer(a, sycl::range<1>(11));
}