#include <sycl/sycl.hpp>

void test() {
	sycl::queue myQueue;

	myQueue.submit(
  	[&](sycl::handler& cgh) {
      sycl::local_accessor<int, 1> a_local_acc = sycl::local_accessor<bool>(sycl::range<1>(10), cgh);

      cgh.parallel_for(sycl::nd_range<1>(sycl::range<1>(1),sycl::range<1>(1)), [=] (sycl::nd_item<1> it) {});
  	}
  );
}