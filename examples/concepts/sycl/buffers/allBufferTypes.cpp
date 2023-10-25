#include <sycl/sycl.hpp>

/*@
  requires \pointer(a, 10, write);
   requires \pointer(b, 10, write);
   requires \pointer(c, 10, write);
   requires \pointer(d, 10, write);
   requires \pointer(e, 10, write);
   requires \pointer(f, 10, write);
*/
void main(bool* a, int* b, long* c, double* d, float* e, char* f) { // Does not work when using a[] instead of *a
  sycl::buffer<bool, 1> aBuffer = sycl::buffer<bool, 1>(a, sycl::range<1>(10));
  sycl::buffer<int, 2> bBuffer = sycl::buffer<int, 2>(b, sycl::range<2>(2, 5));
  sycl::buffer<long, 3> cBuffer = sycl::buffer<long, 3>(c, sycl::range<3>(2, 5, 1));
  sycl::buffer<double, 1> dBuffer = sycl::buffer<double, 1>(d, sycl::range<1>(10));
  sycl::buffer<float, 1> dBuffer = sycl::buffer<float, 1>(e, sycl::range<1>(10));
  sycl::buffer<char, 1> dBuffer = sycl::buffer<char, 1>(f, sycl::range<1>(10));
}