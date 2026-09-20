#include <sycl/sycl.hpp>

/*@
    context_everywhere N > 0 && T > N && T%N == 0 && N%sycl::h::warp_sizes()==0;
@*/
int foo(sycl::queue q, int T, int N, int* fx) {
    q.submit([&](sycl::handler& h)
    {
        h.parallel_for(sycl::nd_range<1>(sycl::range<1>(T), sycl::range<1>(N)),
        [=](sycl::nd_item<1> it) {
            sycl::sub_group sg = it.get_sub_group();
        });});
}