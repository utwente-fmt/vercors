#include <sycl/sycl.hpp>

//@ requires \pointer(a, 12, write);
//@ requires (\forall int i; i >= 0 && i < 12; a[i] == i);
void test(int* a) {
	sycl::queue myQueue;
	{
    sycl::buffer<int, 1> aBuffer = sycl::buffer(a, sycl::range<1>(12));

    myQueue.submit(
      [&](sycl::handler& cgh) {
        sycl::accessor<int, 1, sycl::access_mode::read> a_accessor = sycl::accessor(aBuffer, cgh, sycl::read_only);
        sycl::accessor<int, 1> aa_accessor = sycl::accessor(aBuffer, cgh, sycl::read_write);

        cgh.parallel_for(sycl::range<1>(12),
        /*@
          context it.get_id(0) < a_accessor.get_range().get(0);
          context Perm(aa_accessor[it.get_id(0)], write);
        */
        [=] (sycl::item<1> it) {
          // Not allowed in SYCL, but is allowed in VerCors,
          // because a_accessor and aa_accessor are both mapped to the same buffer,
          // to which there is write access because of aa_accessor
          a_accessor[it.get_id(0)] = 10;
        });
      }
    );
  }

}