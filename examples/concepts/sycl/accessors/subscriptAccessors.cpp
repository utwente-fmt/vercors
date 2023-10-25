#include <sycl/sycl.hpp>

/*@ 
  requires \pointer(a, 10, write);
  requires (\forall int i; i >= 0 && i < 10; \pointer(a[i], 8, write));
  requires (\forall int i, int j; i >= 0 && i < 10 && j >= 0 && j < 8; \pointer(a[i][j], 6, write));
*/
void main(bool* a) {
  sycl::queue myQueue;

  sycl::buffer<bool, 3> aBuffer = sycl::buffer<bool, 3>(a, sycl::range<3>(10, 8, 6));

  myQueue.submit(
  	[&](sycl::handler& cgh) {
      
      sycl::accessor a_accessor = sycl::accessor(aBuffer, cgh);

      cgh.parallel_for(sycl::range<3>(10, 6, 8), [=] (sycl::item<3> it) {
        int b = a_accessor[it.get_id(0)][it.get_id(1)][it.get_id(2)];
      });
  	}
  );
}