#include <sycl/sycl.hpp>
/*

    This example is obsolete. Due to fixes in the scoping, the missing annotation here is not needed anymore.
    I call it, "missing by design"
*/
/*@
  requires \pointer(a, 12, write);
*/
void test(int* a) {
	sycl::queue myQueue;

  sycl::buffer<int, 3> aBuffer = sycl::buffer(a, sycl::range<3>(2, 3, 2));

	myQueue.submit(
  	[&](sycl::handler& cgh) {

      sycl::accessor<int, 3, sycl::access_mode::read> a_accessor = sycl::accessor(aBuffer, cgh, sycl::read_only);

      cgh.parallel_for(sycl::range<1>(1),
        /*@
          requires 1 < a_accessor.get_range().get(0);
          requires 2 < a_accessor.get_range().get(1);
          requires (\forall* int x = 0 .. a_accessor.get_range().get(0), int y = 0 .. a_accessor.get_range().get(1), int z = 0 .. a_accessor.get_range().get(2);
            Perm(a_accessor[x][y][z], read));
        */
        [=] (sycl::item<1> it) { // Bound requirement for the 3rd index is missing, so cannot verify
          int a = a_accessor[1][2][1];
        }
      );
  	}
  );
}