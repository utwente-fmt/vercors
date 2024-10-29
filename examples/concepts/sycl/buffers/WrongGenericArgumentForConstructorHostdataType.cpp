#include <sycl/sycl.hpp>

//@ requires \pointer(a, 10, write);
void test(bool a[]) {
  sycl::buffer<int>(a, sycl::range<1>(10));
}