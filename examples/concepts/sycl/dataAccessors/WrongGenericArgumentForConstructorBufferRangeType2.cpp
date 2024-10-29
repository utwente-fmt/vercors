#include <sycl/sycl.hpp>

//@ requires \pointer(a, 12, write);
void test(int* a) {
	sycl::queue myQueue;
  sycl::buffer<int, 2> aBuffer = sycl::buffer(a, sycl::range<2>(6,2));

	myQueue.submit(
  	[&](sycl::handler& cgh) {
  	  // generic arg <int> will be expanded to <int, 1, sycl::access_mode::read_write>
      sycl::accessor<int>(aBuffer, cgh, sycl::read_write);

      cgh.parallel_for(sycl::range<1>(1), [=] (sycl::item<1> it) {});
  	}
  );
}