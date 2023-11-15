#include <sycl/sycl.hpp>

/*@ 
  requires \pointer(a, 10, write);
*/
void main(int* a) {
	sycl::queue myQueue;

  sycl::buffer<int, 1> aBuffer = sycl::buffer(a, sycl::range<1>(10));

	myQueue.submit(
  	[&](sycl::handler& cgh) {
      
      sycl::accessor<int, 1> a_accessor = sycl::accessor(aBuffer, cgh, sycl::read_only);

      cgh.parallel_for(sycl::range<1>(10),
        /*@
          context it.get_id(0) < a_accessor.get_range().get(0);
        */
        [=] (sycl::item<1> it) {
          a_accessor[it.get_id(0)] = 10; // Shloud not be allowed
        }
      );
  	}
  );
}