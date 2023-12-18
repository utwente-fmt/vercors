#include <sycl/sycl.hpp>

void test() {
	sycl::queue myQueue;

	sycl::event myEvent = myQueue.submit(
	[&](sycl::handler& cgh) {
		// Dimensions of range and item should match
		cgh.parallel_for(sycl::range<3>(6,4,2), [=] (sycl::item<2> it) {});
	});
}