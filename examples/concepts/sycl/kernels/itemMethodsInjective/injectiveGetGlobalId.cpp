#include <sycl/sycl.hpp>

//@ requires \pointer(a, 10, write);
void test(int* a) {
	sycl::queue myQueue;
  sycl::buffer<int, 1> aBuffer = sycl::buffer(a, sycl::range<1>(10));

  myQueue.submit(
    [&](sycl::handler& cgh) {
      sycl::accessor<int, 1> a_accessor = sycl::accessor(aBuffer, cgh, sycl::read_write);
      cgh.parallel_for(sycl::nd_range<1>(sycl::range<1>(10), sycl::range<1>(5)),
        /*@
          context it.get_global_id(0) < a_accessor.get_range().get(0);
          context Perm(a_accessor[it.get_global_id(0)], write);
          ensures a_accessor[it.get_global_id(0)] == 10;
        */
        [=] (sycl::nd_item<1> it) {
          a_accessor[it.get_global_id(0)] = 10;
        }
      );
    }
  );
}