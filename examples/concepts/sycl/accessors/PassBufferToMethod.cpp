#include <sycl/sycl.hpp>

/*@ 
  requires \pointer(a, 10, write);
*/
void main(int* a) {
  sycl::buffer<int, 1> aBuffer = sycl::buffer<int, 1>(a, sycl::range<1>(10));
  runKernel(aBuffer); // Passing buffer to another method is not supported.
}

void runKernel(sycl::buffer<int, 1> buffer) {
  sycl::queue myQueue;
  myQueue.submit(
    [&](sycl::handler& cgh) {

      sycl::accessor<int, 1> a_accessor = sycl::accessor(buffer, cgh, sycl::read_write);

      cgh.parallel_for(sycl::range<1>(10), [=] (sycl::item<1> it) {
        a_accessor[it.get_id(0)] = 10;
      });
    }
  );
}