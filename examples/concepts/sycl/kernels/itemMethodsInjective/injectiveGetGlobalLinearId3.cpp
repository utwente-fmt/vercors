#include <sycl/sycl.hpp>

//@ requires \pointer(a, 1000, write);
void test(int* a, int* b) {
	sycl::queue myQueue;
	sycl::buffer<int, 1> aBuffer = sycl::buffer(a, sycl::range<1>(1000));

  myQueue.submit(
    [&](sycl::handler& cgh) {
      sycl::accessor<int, 1> a_accessor = sycl::accessor(aBuffer, cgh, sycl::read_write);
      cgh.parallel_for(sycl::nd_range<3>(sycl::range<3>(10, 10, 10), sycl::range<3>(5,5,5)),
        /*@
          context it.get_global_linear_id() < a_accessor.get_range().get(0);
          context Perm(a_accessor[it.get_global_linear_id()], write);
          ensures a_accessor[it.get_global_linear_id()] == 10;
        */
        [=] (sycl::nd_item<3> it) {
          a_accessor[it.get_global_linear_id()] = 10;
        }
      );
    }
  );

}