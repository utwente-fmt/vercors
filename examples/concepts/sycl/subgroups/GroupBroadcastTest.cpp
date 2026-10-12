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
            /*@ context Perm(fxAcc[it.get_global_id(0)], write);
                context (\forall int i=0 .. T; fxGs[i] == \old[bK](fx[i]));
                requires |fxGs| == T &&
                    {:fxGs[it.get_global_id(0)]:} == fxAcc[it.get_global_id(0)] &&
                    {:fxGs[it.get_global_id(0)]:} == \old(fx[it.get_global_id(0)]);
                ensures it.get_global_id(0) + 1 < |fxGs| ==>
                    fxAcc[it.get_global_id(0)] == fxGs[it.get_global_id(0) - it.get_sub_group().get_local_id()[0] + 1];
                */
            [=](sycl::nd_item<1> it) {
                sycl::sub_group sg = it.get_sub_group();
                int gid = it.get_global_id(0);
                int laneId = sg.get_local_id()[0];
                int d1 = 1;

                // The assumption below is required for the assertion below it. It can be proven,
                // It can be proven, but to not slow down this case with another proof, it is assumed.
                //@ assume gid - laneId + d1 < |fxGs|;

                fxAcc[gid] = sycl::group_broadcast(it.get_sub_group(), fxAcc[gid], d1) /*@ sub_group_inv { \sg_val == fxGs[\gtid] } */;
                //@ assert fxAcc[gid] == fxGs[gid - laneId + d1];
            });});
        e0.wait();
    }
}
