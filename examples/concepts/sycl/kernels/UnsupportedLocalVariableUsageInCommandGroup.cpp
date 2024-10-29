#include <sycl/sycl.hpp>

void test() {
	sycl::queue myQueue;
	int a = 5;

	sycl::event myEvent = myQueue.submit(
	[&](sycl::handler& cgh) {
	  // Cannot use local variable 'a' in local accessor constructor, as it is inside the command group
		sycl::local_accessor<int, 1> a_local_acc = sycl::local_accessor<int>(sycl::range<1>(a), cgh);

		cgh.parallel_for(sycl::nd_range<1>(sycl::range<1>(6),sycl::range<1>(2)), [=] (sycl::nd_item<1> it) {});
	});
}