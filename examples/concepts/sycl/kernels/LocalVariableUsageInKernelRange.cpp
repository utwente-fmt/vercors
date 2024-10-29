#include <sycl/sycl.hpp>

void test() {
	sycl::queue myQueue;
	int a = 5;

	sycl::event myEvent = myQueue.submit(
	[&](sycl::handler& cgh) {
		cgh.parallel_for(sycl::range<2>(a, a+6), [=] (sycl::item<2> it) {});
	});
}