#include <sycl/sycl.hpp>

//@ requires \pointer(a, 10, write);
void test(int* a) {
	sycl::queue myQueue;
  sycl::buffer<int, 1> aBuffer = sycl::buffer(a, sycl::range<1>(10));

  myQueue.submit(
      [&](sycl::handler& cgh) {
        sycl::accessor<int, 1> a_accessor = sycl::accessor(aBuffer, cgh, sycl::read_write);
        cgh.parallel_for(sycl::nd_range<1>(sycl::range<1>(10), sycl::range<1>(1)),
          /*@
            context it.get_group_linear_id() < a_accessor.get_range().get(0);
            context it.get_local_range(0) == 1;
            context Perm(a_accessor[it.get_group_linear_id()], write);
            ensures a_accessor[it.get_group_linear_id()] == 10;
          */
          [=] (sycl::nd_item<1> it) {
            a_accessor[it.get_group_linear_id()] = 10;
          }
        );
      }
    );

}