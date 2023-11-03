#include <sycl/sycl.hpp>

void main() {
	sycl::queue myQueue;

	sycl::event myEvent = myQueue.submit(
	[&](sycl::handler& cgh) {
		// Only one kernel is allowed in a command group
		cgh.parallel_for(sycl::range<3>(6,4,2), [=] (sycl::item<3> it) {});
		cgh.parallel_for(sycl::range<1>(6), [=] (sycl::item<1> it) {});
	});
}