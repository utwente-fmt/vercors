#include <sycl/sycl.hpp>

void test() {
	sycl::queue myQueue;

	sycl::event myEvent = myQueue.submit(
  	[&](sycl::handler& cgh) {
  		cgh.parallel_for(sycl::nd_range<3>(sycl::range<3>(6,4,4), sycl::range<3>(3,2,1)),
				[=] (sycl::nd_item<3> it) {
					int a = it.get_global_id(0);
					int b = it.get_global_linear_id();
					int c = it.get_global_range(0);

					int d = it.get_local_id(0);
					int e = it.get_local_linear_id();
					int f = it.get_local_range(0);

					int g = it.get_group_id(0);
					int h = it.get_group_linear_id();
					int i = it.get_group_range(0);
				}
  		);
  	}
  );

	int a = 5;
	myEvent.wait();
}