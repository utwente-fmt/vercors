#include <sycl/sycl.hpp>

void test2() {
	sycl::queue myQueue;
//	sycl::handler myHandler;

//	sycl::range<3> myRange;

	sycl::event myEvent = myQueue.submit(
	/*@ requires true; */
	[&](sycl::handler& cgh) {
		cgh.parallel_for(sycl::range<3>(6,4,2), // global range
		/*@ requires it.get_id(0) > -1;  */
		[=] (sycl::item<3> it) {
		//[kernel code]
			int a = it.get_id(0) + it.get_id(1) + it.get_id(2) + 3;
			int b = a + 45;
			int c = it.get_range(0);
			int d = it.get_linear_id();
		});
	});
	int b = 20;
	myEvent.wait();
}