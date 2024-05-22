#include <sycl/sycl.hpp>

void test() {
	sycl::queue myQueue;

	sycl::event myEvent = myQueue.submit(
		[&](sycl::handler& cgh) {
			cgh.parallel_for(sycl::range<3>(6,4,2),
				/*@
					requires it.get_range(0) == 6;
					ensures it.get_linear_id() > -1;
				*/
				[=] (sycl::item<3> it) {
					int a = it.get_id(1);
					//@ assert a < it.get_range(1) && a >= 0;
					int b = it.get_linear_id();
					//@ assert b < it.get_range(0) * it.get_range(1) * it.get_range(2);
					//@ assert b == it.get_id(2) + (it.get_id(1) * it.get_range(2)) + (it.get_id(0) * it.get_range(1) * it.get_range(2));
				}
			);
		}
	);
	int a = 5;
	myEvent.wait();
}