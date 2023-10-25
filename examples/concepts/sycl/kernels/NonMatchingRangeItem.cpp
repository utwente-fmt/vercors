#include <sycl/sycl.hpp>

void main() {
	sycl::queue myQueue;

	sycl::event myEvent = myQueue.submit(
	[&](sycl::handler& cgh) {
		// For range we need an item not nd_item
		cgh.parallel_for(sycl::range<3>(6,4,2), [=] (sycl::nd_item<3> it) {});
	});
}