#include <sycl/sycl.hpp>

void main() {
	sycl::queue myQueue;

	sycl::event myEvent = myQueue.submit(
	[&](sycl::handler& cgh) {
		// 6 in global range is not divisible by 0 in local range as you cannot divide by zero
		cgh.parallel_for(sycl::nd_range<2>(sycl::range<2>(6,4), sycl::range<2>(0,2)), [=] (sycl::nd_item<2> it) {});
	});
}