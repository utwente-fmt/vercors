#include <sycl/sycl.hpp>

void test() {
	sycl::queue myQueue;

	sycl::event myEvent = myQueue.submit(
	[&](sycl::handler& cgh) {
		int a = 5;
		cgh.parallel_for(sycl::range<2>(6,2), [=] (sycl::item<2> it) {});
	});
}