#include <sycl/sycl.hpp>

void main() {
	sycl::queue myQueue;

	sycl::event myEvent = myQueue.submit(
	[&](sycl::handler& cgh) {
		// 6 in global range is not divisible by 4 in local range
		cgh.parallel_for(sycl::nd_range<2>(sycl::range<2>(6,4), sycl::range<2>(4,2)), [=] (sycl::nd_item<2> it) {});
	});
}