#include <sycl/sycl.hpp>

/*@
  requires \pointer(a, 6, write);
*/
void main(int* a) {
	sycl::queue myQueue;

  sycl::buffer<int, 2> aBuffer = sycl::buffer(a, sycl::range<1>(2, 3));

	myQueue.submit(
  	[&](sycl::handler& cgh) {

      sycl::accessor<int, 2> a_accessor = sycl::accessor(aBuffer, cgh, sycl::read_only);

      cgh.parallel_for(sycl::range<2>(2, 3),
        [=] (sycl::item<2> it) {
          a_accessor.get_range().get(2); // There are only 2 dimensions, so should error
        }
      );
  	}
  );
}