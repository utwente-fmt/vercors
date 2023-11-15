#include <sycl/sycl.hpp>

void test() {
	sycl::queue myQueue;

  myQueue.submit(
    [&](sycl::handler& cgh) {
      sycl::local_accessor<int, 1> a_local_acc = sycl::local_accessor(sycl::range<1>(20), cgh);

      cgh.parallel_for(sycl::range<1>(20),
        [=] (sycl::item<1> it) {
          a_local_acc[it.get_id(0)] = 10; // Not allowed because it is a basic kernel
        }
      );
    }
  );

}