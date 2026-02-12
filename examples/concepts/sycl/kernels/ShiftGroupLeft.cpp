#include <sycl/sycl.hpp>


#include <sycl/sycl.hpp>

/*@
  requires \pointer(a, 10, write);
*/
void test(int a[]) {
  sycl::queue q;

  {
    sycl::buffer<int, 1> buf = sycl::buffer(a, sycl::range<1>(10));
//    sycl::buffer<int, 1> aBuffer = sycl::buffer(a, sycl::range<1>(12));


    q.submit([&](sycl::handler& h) {
      sycl::accessor<int, 1> acc = sycl::accessor(buf, h, sycl::read_write);

      h.parallel_for(
        sycl::nd_range<1>(sycl::range<1>(8),sycl::range<1>(8)),
        [=](sycl::nd_item<1> item) {
          sycl::sub_group sg = item.get_sub_group();

          int x = item.get_global_id(0);

          // shift value from the work-item to the right
          int y = sycl::shift_group_left(sg, x, 1) /*@ given { P = true } @*/;
          y = sycl::shift_group_left(sg, x, 1) /*@ given { P = true } @*/;

          acc[item.get_global_id(0)] = y;
        });
    });
  }

}

/*@
opaque pure int warpSize()=32;
*/