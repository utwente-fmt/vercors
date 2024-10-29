#include <sycl/sycl.hpp>

void test() {
	sycl::queue myQueue;

  myQueue.submit(
    [&](sycl::handler& cgh) {
      sycl::local_accessor<int, 1> a_local_acc = sycl::local_accessor<int, 1>(sycl::range<1>(5), cgh);

      cgh.parallel_for(sycl::nd_range<1>(sycl::range<1>(1), sycl::range<1>(1)),
        /*@
          context it.get_local_range(0) == 1;
          context it.get_group_range(0) == 1;
          context Perm(a_local_acc[-1], read);
        */
        [=] (sycl::nd_item<1> it) {
          int x = a_local_acc[-1];
        }
      );
    }
  );

}