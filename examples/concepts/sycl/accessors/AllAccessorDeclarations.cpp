#include <sycl/sycl.hpp>

/*@
  requires \pointer(a, 12, write);
  requires \pointer(b, 12, write);
  requires \pointer(c, 12, write);
  requires \pointer(d, 12, write);
*/
void test(int* a, int* b, int* c, int* d) {
	sycl::queue myQueue;

  sycl::buffer<int, 1> aBuffer = sycl::buffer(a, sycl::range<1>(12));
  sycl::buffer<int, 2> bBuffer = sycl::buffer(b, sycl::range<2>(3, 4));
  sycl::buffer<int, 3> cBuffer = sycl::buffer(c, sycl::range<3>(2, 3, 2));
  sycl::buffer<int, 1> dBuffer = sycl::buffer(d, sycl::range<1>(12));

	myQueue.submit(
  	[&](sycl::handler& cgh) {

      sycl::accessor<int, 1, sycl::access_mode::read> a_accessor = sycl::accessor(aBuffer, cgh, sycl::read_only);
      sycl::accessor<int, 2> b_accessor = sycl::accessor(bBuffer, cgh, sycl::read_write);
      sycl::accessor<int, 2, sycl::access_mode::read_write> bb_accessor = sycl::accessor<int, 2>(bBuffer, cgh, sycl::read_write);
      sycl::accessor<int, 2, sycl::access_mode::read_write> bbb_accessor = sycl::accessor<int, 2, sycl::access_mode::read_write>(bBuffer, cgh, sycl::read_write);
      sycl::accessor<int, 3, sycl::access_mode::read> c_accessor = sycl::accessor(cBuffer, cgh, sycl::read_only);
      sycl::accessor<int, 3, sycl::access_mode::read> ccc_accessor = sycl::accessor<int, 3, sycl::access_mode::read>(cBuffer, cgh, sycl::read_only);
      sycl::accessor<int, 1> d_accessor = sycl::accessor<int>(dBuffer, cgh, sycl::read_write);

      cgh.parallel_for(sycl::range<1>(1),
        /*@
          context 1 < a_accessor.get_range().get(0);
          context 1 < b_accessor.get_range().get(0);
          context 2 < b_accessor.get_range().get(1);
          context 1 < c_accessor.get_range().get(0);
          context 2 < c_accessor.get_range().get(1);
          context 1 < c_accessor.get_range().get(2);
          context it.get_range(0) == 1;
          context Perm(a_accessor[1], read);
          context Perm(b_accessor[1][2], write);
          context Perm(c_accessor[1][2][1], read);
        */
        [=] (sycl::item<1> it) {
//          b_accessor[1][2] = c_accessor[1][2][1] + a_accessor[1];
        }
      );
  	}
  );
}