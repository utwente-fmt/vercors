#include <sycl/sycl.hpp>

void test() {
	sycl::queue myQueue;

	sycl::event myEvent = myQueue.submit(
	[&](sycl::handler& cgh) {

		sycl::queue myQueue1; // Not allowed

		cgh.parallel_for(sycl::range<2>(6,2), [=] (sycl::item<2> it) {});
	});
}