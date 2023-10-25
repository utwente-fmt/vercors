#include <sycl/sycl.hpp>

/*@ 
  requires \pointer(a, 10, write);
*/
void main(bool* a) {
  sycl::buffer<bool, 1> aBuffer = sycl::buffer<bool, 1>(a, sycl::range<1>(10));

  a[5] = true;
}