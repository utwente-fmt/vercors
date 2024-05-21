#include <sycl/sycl.hpp>

//@ requires \pointer(a, 6, write);
void test(int* a) {
	sycl::queue myQueue;
  sycl::buffer<int, 1> aBuffer = sycl::buffer(a, sycl::range<1>(6));

	myQueue.submit(
  	[&](sycl::handler& cgh) {
      sycl::accessor<int, 1, sycl::access_mode::read> a_accessor = sycl::accessor(aBuffer, cgh, sycl::read_only);

      cgh.parallel_for(sycl::range<1>(1),
        /*@
          context it.get_range(0) == 1;
          context a_accessor.get_range().get(0) == 6;
          context Perm(a_accessor[10], read);
        */
        [=] (sycl::item<1> it) {
          int x = a_accessor[10]; // range is 0-5, so should error
        }
      );
  	}
  );
}