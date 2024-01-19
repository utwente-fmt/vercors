#include <sycl/sycl.hpp>

/*@
  requires \pointer(a, 12, write);
*/
void test(int* a) {
	sycl::queue myQueue;

  sycl::buffer<int, 1> aBuffer = sycl::buffer<int, 1>(a, sycl::range<1>(12));

  // Two kernels writing to the same buffer wait on each other

	myQueue.submit(
  	[&](sycl::handler& cgh) {
      sycl::accessor<int, 1> a_accessor = sycl::accessor(aBuffer, cgh, sycl::read_write);

      cgh.parallel_for(sycl::range<1>(1), [=] (sycl::item<1> it) {});
  	}
  );

  myQueue.submit(
    [&](sycl::handler& cgh) {
      sycl::accessor<int, 1> a_accessor = sycl::accessor(aBuffer, cgh, sycl::read_write);

      cgh.parallel_for(sycl::range<1>(1), [=] (sycl::item<1> it) {});
    }
  );
}