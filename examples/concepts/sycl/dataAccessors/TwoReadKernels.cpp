#include <sycl/sycl.hpp>

/*@
  requires \pointer(a, 12, write);
*/
void test(int* a) {
	sycl::queue myQueue;
  sycl::buffer<int, 1> aBuffer = sycl::buffer<int, 1>(a, sycl::range<1>(12));

	myQueue.submit(
  	[&](sycl::handler& cgh) {
      sycl::accessor<int, 1, sycl::access_mode::read> a_accessor = sycl::accessor(aBuffer, cgh, sycl::read_only);

      cgh.parallel_for(sycl::range<1>(1), [=] (sycl::item<1> it) {});
  	}
  );

  myQueue.submit(
    [&](sycl::handler& cgh) {
      sycl::accessor<int, 1, sycl::access_mode::read> a_accessor = sycl::accessor(aBuffer, cgh, sycl::read_only);

      cgh.parallel_for(sycl::range<1>(1), [=] (sycl::item<1> it) {});
    }
  );
}