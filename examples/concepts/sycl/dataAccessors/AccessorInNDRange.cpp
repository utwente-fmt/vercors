#include <sycl/sycl.hpp>

/*@
  context \pointer(a, 10, write);
*/
void test(int* a) {
	sycl::queue myQueue;

  {
    sycl::buffer<int, 1> aBuffer = sycl::buffer(a, sycl::range<1>(10));

    sycl::event e = myQueue.submit(
      [&](sycl::handler& cgh) {

        sycl::accessor<int, 1> a_accessor = sycl::accessor(aBuffer, cgh, sycl::read_write);

        cgh.parallel_for(sycl::nd_range<1>(sycl::range<1>(10), sycl::range<1>(2)),
          /*@
            context it.get_global_id(0) < a_accessor.get_range().get(0);
            context Perm(a_accessor[it.get_global_id(0)], write);
            ensures a_accessor[it.get_global_id(0)] == 10;
          */
          [=] (sycl::nd_item<1> it) {
            a_accessor[it.get_global_id(0)] = 10;
          }
        );
      }
    );
    e.wait();
  } // Leaving scope, which destroys aBuffer, which waits on the kernel to terminate as it uses aBuffer

  /*@ assert (\forall int l=0 .. 2, int g=0 .. 5; {:tr0(g,l):}; a[sycl::linearize2(g, l,5, 2)] == 10);
      assert (\forall int l=0 .. 2, int g=0 .. 5; tr0(g,l); {:a[g*2+l]:} == 10);
      assert (\forall int i=0 .. 10; a[i] == 10); */
}

/*@
ensures \result;
pure bool tr0(int g, int l);
*/