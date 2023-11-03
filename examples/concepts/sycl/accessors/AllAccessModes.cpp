#include <sycl/sycl.hpp>

/*@
  requires \pointer(a, 12, write);
  requires \pointer(b, 12, write);
*/
void main(int* a, int* b) {
	sycl::queue myQueue;

  sycl::buffer<int, 2> aBuffer = sycl::buffer(a, sycl::range<2>(3, 4));
  sycl::buffer<int, 3> bBuffer = sycl::buffer(b, sycl::range<3>(2, 3, 2));

	myQueue.submit(
  	[&](sycl::handler& cgh) {

      sycl::accessor<int, 2> a_accessor = sycl::accessor(aBuffer, cgh, sycl::read_write);
      sycl::accessor<int, 3> b_accessor = sycl::accessor(bBuffer, cgh, sycl::read_only);

      cgh.parallel_for(sycl::range<1>(1),
        /*@
          context 1 < a_accessor.get_range().get(0);
          context 2 < a_accessor.get_range().get(1);
          context 1 < b_accessor.get_range().get(0);
          context 2 < b_accessor.get_range().get(1);
          context 1 < b_accessor.get_range().get(2);
          context Perm(a_accessor[1][2], write);
          context Perm(b_accessor[1][2][1], read);
        */
        [=] (sycl::item<1> it) {
          a_accessor[1][2] = b_accessor[1][2][1];
        }
      );
  	}
  );
}