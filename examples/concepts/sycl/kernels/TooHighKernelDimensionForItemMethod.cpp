#include <sycl/sycl.hpp>

void test() {
	sycl::queue myQueue;

	sycl::event myEvent = myQueue.submit(
	[&](sycl::handler& cgh) {
		cgh.parallel_for(sycl::range<3>(6,4,2), [=] (sycl::item<3> it) {
			int a = it.get_id(3); // argument should be between 0 and 2
		});
	});
}