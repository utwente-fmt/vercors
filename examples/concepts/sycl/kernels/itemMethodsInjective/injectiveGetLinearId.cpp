#include <sycl/sycl.hpp>

//@ requires \pointer(a, 1000, write);
void test(int* a, int* b) {
	sycl::queue myQueue;
  sycl::buffer<int, 1> aBuffer = sycl::buffer(a, sycl::range<1>(1000));

  myQueue.submit(
      [&](sycl::handler& cgh) {
        sycl::accessor<int, 1> a_accessor = sycl::accessor(aBuffer, cgh, sycl::read_write);
        cgh.parallel_for(sycl::range<1>(1000),
          /*@
            context it.get_linear_id() < a_accessor.get_range().get(0);
            context Perm(a_accessor[it.get_linear_id()], write);
            ensures a_accessor[it.get_linear_id()] == 10;
          */
          [=] (sycl::item<1> it) {
            a_accessor[it.get_linear_id()] = 10;
          }
        );
      }
    );

  myQueue.submit(
    [&](sycl::handler& cgh) {
      sycl::accessor<int, 1> a_accessor = sycl::accessor(aBuffer, cgh, sycl::read_write);
      cgh.parallel_for(sycl::range<2>(100, 10),
        /*@
          context it.get_linear_id() < a_accessor.get_range().get(0);
          context Perm(a_accessor[it.get_linear_id()], write);
          ensures a_accessor[it.get_linear_id()] == 10;
        */
        [=] (sycl::item<2> it) {
          a_accessor[it.get_linear_id()] = 10;
        }
      );
    }
  );

  myQueue.submit(
    [&](sycl::handler& cgh) {
      sycl::accessor<int, 1> a_accessor = sycl::accessor(aBuffer, cgh, sycl::read_write);
      cgh.parallel_for(sycl::range<3>(10, 10, 10),
        /*@
          context it.get_linear_id() < a_accessor.get_range().get(0);
          context Perm(a_accessor[it.get_linear_id()], write);
          ensures a_accessor[it.get_linear_id()] == 10;
        */
        [=] (sycl::item<3> it) {
          a_accessor[it.get_linear_id()] = 10;
        }
      );
    }
  );

}