#include <sycl/sycl.hpp>

/*@
  requires \pointer(a, 12, write);
*/
void main(int* a) {
	sycl::queue myQueue;

  sycl::buffer<int, 3> aBuffer = sycl::buffer(a, sycl::range<3>(2, 3, 2));

	myQueue.submit(
  	[&](sycl::handler& cgh) {

      sycl::accessor<int, 3> a_accessor = sycl::accessor(aBuffer, cgh, sycl::read_only);

      cgh.parallel_for(sycl::range<1>(1),
        /*@
          requires 1 < a_accessor.get_range().get(0);
          requires 2 < a_accessor.get_range().get(1);
        */
        [=] (sycl::item<1> it) { // Bound requirement for the 3rd index is missing, so cannot verify
          int a = a_accessor[1][2][1];
        }
      );
  	}
  );
}