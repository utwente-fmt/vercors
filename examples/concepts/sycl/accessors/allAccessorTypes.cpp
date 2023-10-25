#include <sycl/sycl.hpp>

/*@ 
  requires \pointer(a, 10, write);
  requires \pointer(b, 10, write);
  requires \pointer(c, 10, read);
  requires \pointer(d, 10, write);
*/
void main(int* a, int* b, int* c, int* d) {
	sycl::queue myQueue;

  sycl::buffer<int, 1> aBuffer = sycl::buffer<int, 1>(a, sycl::range<1>(10));
  sycl::buffer<int, 1> bBuffer = sycl::buffer<int, 1>(b, sycl::range<1>(10));
  sycl::buffer<int, 1> cBuffer = sycl::buffer<int, 1>(c, sycl::range<1>(10));
  sycl::buffer<int, 1> dBuffer = sycl::buffer<int, 1>(d, sycl::range<1>(10));

	myQueue.submit(
  	[&](sycl::handler& cgh) {
      
      sycl::accessor a_accessor = sycl::accessor(aBuffer, cgh);
      sycl::accessor b_accessor = sycl::accessor(bBuffer, cgh, sycl::read_write);
      sycl::accessor c_accessor = sycl::accessor(cBuffer, cgh, sycl::read_only);
      sycl::accessor d_accessor = sycl::accessor(dBuffer, cgh, sycl::write_only); // Not possible with permissions but can use syntax check

      cgh.parallel_for(sycl::range<1>(1), [=] (sycl::item<1> it) {});
  	}
  );
}