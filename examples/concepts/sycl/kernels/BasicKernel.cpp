#include <sycl/sycl.hpp>

void test() {
	sycl::queue myQueue;

	sycl::event myEvent = myQueue.submit(
		[&](sycl::handler& cgh) {
			cgh.parallel_for(sycl::range<3>(6,4,2),
				/*@
					requires it.get_id(0) > -1;
					ensures it.get_linear_id() > -1;
				*/
				[=] (sycl::item<3> it) {
					int a = it.get_id(1);
					//@ assert a < 6 && a >= 0;
					int b = it.get_linear_id();
					//@ assert b < 48 && b >= 0;
					int c = it.get_range(0);
					//@ assert c == 6;
				}
			);
		}
	);
	int a = 5;
	myEvent.wait();
}