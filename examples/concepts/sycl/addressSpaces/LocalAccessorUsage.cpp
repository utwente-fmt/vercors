#include <sycl/sycl.hpp>

void test() {
	sycl::queue myQueue;

  myQueue.submit(
    [&](sycl::handler& cgh) {
      sycl::local_accessor<int, 1> a_local_acc = sycl::local_accessor<int, 1>(sycl::range<1>(5), cgh);

      cgh.parallel_for(sycl::nd_range<1>(sycl::range<1>(20), sycl::range<1>(5)),
        /*@
          context Perm(a_local_acc[it.get_local_id(0)], write);
          ensures a_local_acc[it.get_local_id(0)] == 10;
        */
        [=] (sycl::nd_item<1> it) {
          a_local_acc[it.get_local_id(0)] = 10;
        }
      );
    }
  );

}