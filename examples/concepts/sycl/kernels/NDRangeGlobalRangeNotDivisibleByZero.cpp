#include <sycl/sycl.hpp>

void main() {
	sycl::queue myQueue;

	sycl::event myEvent = myQueue.submit(
  	[&](sycl::handler& cgh) {
  		// To make work-groups, the global range is divided by the local range, so it cannot be 0
  		cgh.parallel_for(sycl::nd_range<3>(sycl::range<3>(6,4,4), sycl::range<3>(3,0,1)),
				[=] (sycl::nd_item<3> it) {}
  		);
  	}
  );

	myEvent.wait();
}