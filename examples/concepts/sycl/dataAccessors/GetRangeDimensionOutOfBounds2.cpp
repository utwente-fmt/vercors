#include <sycl/sycl.hpp>

//@ requires \pointer(a, 6, write);
void test(int* a) {
	sycl::queue myQueue;

  sycl::buffer<int, 2> aBuffer = sycl::buffer(a, sycl::range<2>(2, 3));

	myQueue.submit(
  	[&](sycl::handler& cgh) {

      sycl::accessor<int, 2, sycl::access_mode::read> a_accessor = sycl::accessor(aBuffer, cgh, sycl::read_only);

      cgh.parallel_for(sycl::range<2>(2, 3),
        [=] (sycl::item<2> it) {
          a_accessor.get_range().get(-1); // Dimension cannot be negative
        }
      );
  	}
  );
}