#include <sycl/sycl.hpp>

/*@
  context size >= 0;
  context \pointer(a, size, write);
  context \pointer(b, size, write);
  context \pointer(c, size, write);
  ensures (\forall int i; 0 <= i && i < size; c[i] == a[i] + b[i]);
@*/
void vector_add(sycl::queue q, int size, int a[], int b[], int c[]) {
  sycl::buffer<int, 1> a_buf = sycl::buffer(a, sycl::range<1>(size));
  sycl::buffer<int, 1> b_buf = sycl::buffer(b, sycl::range<1>(size));
  sycl::buffer<int, 1> c_buf = sycl::buffer(c, sycl::range<1>(size));

  q.submit([&](sycl::handler& h) {
    sycl::accessor<int, 1, sycl::access_mode::read> a_acc = sycl::accessor(a_buf, h, sycl::read_only);
    sycl::accessor<int, 1, sycl::access_mode::read> b_acc = sycl::accessor(b_buf, h, sycl::read_only);
    sycl::accessor<int, 1> c_acc = sycl::accessor(c_buf, h, sycl::read_write);

    h.parallel_for(sycl::range<1>(size),
      /*@
        context it.get_id(0) < a_acc.get_range().get(0);
        context Perm(a_acc[it.get_id(0)], read);
        context it.get_id(0) < b_acc.get_range().get(0);
        context Perm(b_acc[it.get_id(0)], read);
        context it.get_id(0) < c_acc.get_range().get(0);
        context Perm(c_acc[it.get_id(0)], write);
        ensures c_acc[it.get_id(0)] == a_acc[it.get_id(0)] + b_acc[it.get_id(0)];
      @*/
      [=](sycl::item<1> it) {
        c_acc[it.get_id(0)] = a_acc[it.get_id(0)] + b_acc[it.get_id(0)];
      }
    );
  });
}
