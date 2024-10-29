#include <sycl/sycl.hpp>

void test() {
	sycl::queue myQueue;

	myQueue.submit(
  	[&](sycl::handler& cgh) {
  	  // generic arg <int> will be expanded to <int, 1>
      sycl::local_accessor<int>(sycl::range<2>(10), cgh);

      cgh.parallel_for(sycl::nd_range<1>(sycl::range<1>(1),sycl::range<1>(1)), [=] (sycl::nd_item<1> it) {});
  	}
  );
}