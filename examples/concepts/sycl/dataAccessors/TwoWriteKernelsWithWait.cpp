#include <sycl/sycl.hpp>

/*@
  requires \pointer(a, 12, write);
*/
void test(int* a) {
	sycl::queue myQueue;

  sycl::buffer<int, 1> aBuffer = sycl::buffer<int, 1>(a, sycl::range<1>(12));

	sycl::event myEvent = myQueue.submit(
  	[&](sycl::handler& cgh) {
      sycl::accessor<int, 1> a_accessor = sycl::accessor(aBuffer, cgh, sycl::read_write);

      cgh.parallel_for(sycl::range<1>(1), [=] (sycl::item<1> it) {});
  	}
  );

  // Wait for one write kernel to finish before starting the next is allowed as write permission is returned here
  myEvent.wait();

  myQueue.submit(
    [&](sycl::handler& cgh) {
      sycl::accessor<int, 1> a_accessor = sycl::accessor(aBuffer, cgh, sycl::read_write);

      cgh.parallel_for(sycl::range<1>(1), [=] (sycl::item<1> it) {});
    }
  );
}