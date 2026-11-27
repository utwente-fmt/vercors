#include <sycl/sycl.hpp>

/*@
    given seq<int> fxGs;
    context_everywhere N > 0 && T > N && T%N == 0 && N%sycl::h::warp_sizes()==0;
    context \pointer(fx, T, write);
    requires |fxGs| == T && (\forall int i=0 .. T; fxGs[i] == fx[i]);
    ensures |fxGs| == T && (\forall int i=0 .. T; fxGs[i] == \old(fx[i]));
@*/
int smartsum(sycl::queue q, int T, int N, int* fx) {
    //@ label bK;
    {
        sycl::buffer<int, 1> fxBuf = sycl::buffer(fx, sycl::range<1>(T));
        sycl::event e0 = q.submit([&](sycl::handler& h)
        {
            sycl::accessor<int, 1, sycl::access_mode::read_write> fxAcc = sycl::accessor(fxBuf, h, sycl::read_write);

            h.parallel_for(sycl::nd_range<1>(sycl::range<1>(T), sycl::range<1>(N)),
            [=](sycl::nd_item<1> it) {
                sycl::sub_group sg = it.get_sub_group();
                int gid = it.get_global_id(0);
                int laneId = sg.get_local_id()[0];
                int d1 = gid%sg.get_local_range().get(0);
                fxAcc[gid] = sycl::shift_group_left(it.get_sub_group(), fxAcc[gid], d1) /*@ sub_group_inv { true } */;
            });});
        e0.wait();
    }
}

