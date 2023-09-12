#include <sycl/sycl.hpp>

void test2() {
	sycl::queue myQueue;

	sycl::event myEvent = myQueue.submit([&](sycl::handler& cgh) {
		cgh.parallel_for(sycl::range<3>(3,3,3), // global range
		[=] (sycl::item<3> it) {
		//[kernel code]
			int a = it.get_id(1) + 3;
		});
	});
	myEvent.wait();
}