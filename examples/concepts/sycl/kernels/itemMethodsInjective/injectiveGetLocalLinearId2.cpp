#include <sycl/sycl.hpp>

void test() {
	sycl::queue myQueue;

  myQueue.submit(
    [&](sycl::handler& cgh) {
      sycl::local_accessor<int, 1> a_local_acc = sycl::local_accessor<int>(sycl::range<1>(100), cgh);
      cgh.parallel_for(sycl::nd_range<2>(sycl::range<2>(10, 10), sycl::range<2>(10,10)),
        /*@
          context it.get_local_linear_id() < a_local_acc.get_range().get(0);
          context Perm(a_local_acc[it.get_local_linear_id()], write);
          ensures a_local_acc[it.get_local_linear_id()] == 10;
        */
        [=] (sycl::nd_item<2> it) {
          a_local_acc[it.get_local_linear_id()] = 10;
        }
      );
    }
  );
}