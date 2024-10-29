#include <sycl/sycl.hpp>

void test() {
	sycl::queue myQueue;

	myQueue.submit(
  	[&](sycl::handler& cgh) {
      sycl::local_accessor<int, 2> a_local_acc = sycl::local_accessor<int, 2>(sycl::range<2>(6,4), cgh);

      cgh.parallel_for(sycl::nd_range<2>(sycl::range<2>(6,4), sycl::range<2>(3,2)),
        [=] (sycl::nd_item<2> it) {
          a_local_acc.get_range().get(-1); // Negative range is not allowed
        }
      );
  	}
  );
}