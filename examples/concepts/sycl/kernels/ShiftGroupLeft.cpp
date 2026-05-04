#include <sycl/sycl.hpp>

/*
- Check that delta is the same for all.
*/

/*@
    given seq<int> fxGs;
    context_everywhere N > 0 && T > N && T%N == 0 && N%sycl::h::warp_sizes()==0;
    context \pointer(fx, T, write);
    requires |fxGs| == T && (\forall int i=0 .. T; fxGs[i] == fx[i]);
    ensures |fxGs| == T && (\forall int i=0 .. T; fxGs[i] == \old(fx[i]));
    ensures \result == sum(fxGs[0 .. T]);
@*/
int smartsum(sycl::queue q, int T, int N, int* fx) {
    //@ label bK;
    //@ assert true;
    {
        sycl::buffer<int, 1> fxBuf = sycl::buffer(fx, sycl::range<1>(T));
        //@ assert true;

        sycl::event e0 = q.submit([&](sycl::handler& h)
        {
            sycl::accessor<int, 1, sycl::access_mode::read_write> fxAcc = sycl::accessor(fxBuf, h, sycl::read_write);

            h.parallel_for(sycl::nd_range<1>(sycl::range<1>(T), sycl::range<1>(N)),
            /*@
            context Perm(fxAcc[it.get_global_id(0)], write);
            ensures fxAcc == \old(fxAcc);
            context (\forall int i=0 .. T; fxGs[i] == \old[bK](fx[i]));
            requires |fxGs| == T &&
                fxGs[it.get_global_id(0)] == fxAcc[it.get_global_id(0)] &&
                fxGs[it.get_global_id(0)] == \old(fx[it.get_global_id(0)]);

            ensures it.get_sub_group().get_local_id() + it.get_sub_group().get_local_range(0) <= it.get_sub_group().get_local_range(0) ==>
                it.get_global_id(0)+it.get_sub_group().get_local_range(0) <= |fxGs| ==>
                    fxAcc[it.get_global_id(0)] == sum(fxGs[it.get_global_id(0) .. it.get_global_id(0)+it.get_sub_group().get_local_range(0)]);
            */
            [=](sycl::nd_item<1> it) {
            // [=](sycl::nd_item<1> it) [[sycl::reqd_sub_group_size(32)]] {
                //@ assert true;
                sycl::sub_group sg = it.get_sub_group();
                int gid = it.get_global_id(0);
                int laneId = sg.get_local_id();
                int d1 = 1;

                fxAcc[gid] += sycl::shift_group_left(sg, fxAcc[gid], d1)
                /*@ sub_group_inv { \gtid+d1 <= |fxGs| ==> \sg_val == sum(fxGs[\gtid .. \gtid+d1]) } */;


                /*@ assert lemmaSumOverConcat(fxGs[gid .. gid+d1],fxGs[gid+d1 .. gid+d1+d1]);
                assert lemmaSumOverABBCisAC(fxGs, gid, gid+d1,gid+d1, gid+d1+d1);
                assert true; */

                int d2 = 2;
                //@ assert sg.get_local_id() + d1 < sg.get_local_range(0) ==> gid+d2 <= |fxGs| ==> fxAcc[gid] == sum(fxGs[gid .. gid+d2]);

                fxAcc[gid] += sycl::shift_group_left(sg, fxAcc[gid], d2)
                /*@ sub_group_inv { sg.get_local_id() + d1 < sg.get_local_range(0) ==> \gtid+d2 <= |fxGs| ==> \sg_val == sum(fxGs[\gtid .. \gtid+d2]) } */;

                /*@ assert lemmaSumOverConcat(fxGs[gid .. gid+d2],fxGs[gid+d2 .. gid+d2+d2]);
                assert lemmaSumOverABBCisAC(fxGs, gid, gid+d2, gid+d2, gid+d2+d2);
                assert true;*/

                /*@ assert sg.get_local_id() + d2 + d1 < sg.get_local_range(0) ==> gid+d2+d2 <= |fxGs| ==> fxAcc[gid] == sum(fxGs[gid .. gid+d2+d2]);*/
                /*@ assert sg.get_local_id() + sycl::h::exp(2, 2) <= sg.get_local_range(0) ==> gid+d2+d2 <= |fxGs| ==> fxAcc[gid] == sum(fxGs[gid .. gid+d2+d2]);*/

                int dk = 4;
                //@ ghost int k1 = 2;

                /*@ loop_invariant sg.get_local_range(0) %2==0;
                loop_invariant k1 >= 2 && k1 <= sycl::h::wrpsz_pow() && dk == sycl::h::exp(2,k1);
                loop_invariant sg.get_local_range(0) %2==0;
                loop_invariant 4 <= dk && dk <= sg.get_local_range(0);
                loop_invariant dk == sg.get_local_range(0) ==> sycl::h::exp(2,k1) == sg.get_local_range(0);
                loop_invariant dk >= sg.get_local_range(0) ==> dk == sg.get_local_range(0);
                
                loop_invariant (\forall int i=0 .. T; fxGs[i] == \old[bK](fx[i]));
                loop_invariant Perm({:fxAcc[it.get_global_id(0)]:}, write);
                loop_invariant  sg.get_local_id() + sycl::h::exp(2,k1) <= sg.get_local_range(0) ==>
                    gid+dk <= |fxGs| ==> fxAcc[gid] == sum(fxGs[gid .. gid+dk]); */
                for (dk = 4; dk < sg.get_local_range(0); dk = dk * 2) {
                    int sgl_result2 = sycl::shift_group_left(sg, fxAcc[gid], dk)
                    /*@ sub_group_inv { sg.get_local_id() + dk <= sg.get_local_range(0) ==> \gtid+dk <= |fxGs| ==> \sg_val == sum(fxGs[\gtid .. \gtid+dk]) } */;
                    fxAcc[gid] += sgl_result2;

                    /*@ assert lemmaSumOverConcat(fxGs[gid .. gid+dk],fxGs[gid+dk .. gid+dk+dk]);
                    assert lemmaSumOverABBCisAC(fxGs, gid, gid+dk,gid+dk, gid+dk+dk);
                    assert true;*/
                    //@ ghost if (k1+1<sycl::h::wrpsz_pow()) {expMonotonicity(k1+1, sycl::h::wrpsz_pow());}

                    /*@ ghost k1=k1+1; */
                }
                //@ assert true;
            });});
        //@ assert true;
        e0.wait();
        //@ assert true;
    }

    //@ assert reveal intermediateResults(T,N,sycl::h::warp_sizes(),fx,fxGs);
    int result = accumulateResult(T,N,fx)  /*@ given {fxGs=fxGs} */;


    //@ assert true;
    //@ assert result == sum(fxGs[0 .. T]);
    return result;
}

/////////////////////////////////////////
/// Sum related functions and lemma's ///
/////////////////////////////////////////
/*@
ensures |xs| == 0 ==> \result == 0;
ensures |xs| == 1 ==> \result == xs[0];
opaque pure int sum(seq<int> xs) =
    0 < |xs| ? xs[0] + sum(xs[1 .. ]) : 0;


requires |xs| >= 0;
requires |ys| >= 0;
ensures \result;
ensures |xs| == 0 ==> sum(xs + ys) == sum(ys);
ensures |ys| == 0 ==> sum(xs + ys) == sum(xs);
ensures |xs + ys| == |xs| + |ys|;
ensures sum(xs[1 .. ] + ys) == sum(xs[1 .. ]) + sum(ys);
ensures sum(xs) + sum(ys) == sum(xs + ys);
opaque pure bool lemmaSumOverConcat(seq<int> xs, seq<int> ys) =
    0 < |xs| ?
        reveal lemmaSumOverConcat(xs[1 .. ], ys) &&
        xs[1 .. ] + ys == ((xs + ys)[1 .. ]) &&
        reveal sum(xs) + reveal sum(ys) == reveal sum(xs + ys)
        :
        reveal sum(xs) + reveal sum(ys) == reveal sum(xs + ys) &&
        true;

requires a <= b && c <= d && b == c;
ensures \result;
pure bool lemmaSumOverABBCisAC(seq<int> xs, int a, int b, int c, int d) = xs[a .. d] == xs[a .. b] + xs[c .. d];
    
ghost
requires 0 <= k;
requires k < w;
ensures \result;
ensures 2 * sycl::h::exp(2, k) <= sycl::h::exp(2, w);
opaque pure bool expMonotonicity(int k, int w) = 
    (k + 1 == w) ? true : reveal expMonotonicity(k + 1, w);


requires warpsz > 0 && N > 0 && T > N && T%N == 0 && N%warpsz==0;
requires 0 <= gid && gid < T/N;
requires 0 <= lid && lid < N && lid%warpsz == 0; 
requires 0 <= sycl::linearize2(gid, lid, T/N, N) && sycl::linearize2(gid, lid, T/N, N) < T;

ensures \result;
ensures sycl::linearize2(gid, lid, T/N, N)+warpsz <= T;
pure bool idshift(int T, int N,int gid, int lid, int warpsz) = true;


requires k >= 0 && l > 0 && k%l==0;
ensures (k+l)%l==0;
pure bool modwrp(int k, int l) = true;


requires wrpsz > 0 && 0 <= lid && lid < N && lid%wrpsz == 0; 
requires N > 0 &&  N%wrpsz==0;
ensures lid+wrpsz <= N;
pure bool lidshift(int lid, int N, int wrpsz) = true;

requires wsz == sycl::h::warp_sizes() && T > 0 && N > 0 && T > N && T%N == 0 && N%wsz==0 && N > 0 && |fxGs| == T;
requires \pointer(fx, T, read);
opaque pure bool intermediateResults(int T, int N, int wsz, int* fx, seq<int> fxGs) = (\forall int lid1=0 .. N, int gid1=0 .. T/N;
    lid1 % wsz + wsz <= wsz ==> 
    sycl::linearize2(gid1, lid1, T/N, N) + wsz <= |fxGs| ==>
        fx[{:sycl::linearize2(gid1, lid1, T/N, N):}] == sum(fxGs[sycl::linearize2(gid1, lid1, T/N, N) .. sycl::linearize2(gid1, lid1, T/N, N) + wsz]));


requires wsz == sycl::h::warp_sizes() && T > 0 && N > 0 && T > N && T%N == 0 && N%wsz==0 && N > 0  && |fxGs| == T;
requires 0 <= gid && gid < T/N;
requires lid%wsz==0 && 0 <= lid && lid < N;
requires \pointer(fx, T, read);
requires reveal intermediateResults(T,N,wsz,fx,fxGs);
requires lid % wsz + wsz <= wsz;
requires sycl::linearize2(gid, lid, T/N, N) + wsz <= |fxGs|;
ensures \result;
ensures fx[{:sycl::linearize2(gid, lid, T/N, N):}] == sum(fxGs[sycl::linearize2(gid, lid, T/N, N) .. sycl::linearize2(gid, lid, T/N, N) + wsz]);
opaque pure bool intermediateResultsInstantiate(int T, int N, int wsz, int* fx, seq<int> fxGs, int lid, int gid) = true;
*/

/*@
    given seq<int> fxGs;
    context_everywhere T > 0 && N > 0 && T > N && T%N == 0 && N%sycl::h::warp_sizes()==0 && N > 0;
    context_everywhere |fxGs| == T;
    context \pointer(fx, T, 1\2);
    requires intermediateResults(T,N,sycl::h::warp_sizes(),fx,fxGs);
    ensures \result == sum(fxGs[0 .. T]);
@*/
int accumulateResult(int T, int N, int* fx) {
    //@ ghost int wrpSz = sycl::h::warp_sizes();
    //@ label bL;
    int result = 0;
    int gid = 0;
    //@ assert true;

    /*@ loop_invariant wrpSz == sycl::h::warp_sizes();
        loop_invariant N > 0 && T > N && T%N == 0 && N%wrpSz==0;
        loop_invariant 0 <= gid && gid <= T/N;
        loop_invariant T%N==0 && N%wrpSz==0 && |fxGs| == T;
        loop_invariant (gid < T/N ) ==> (0 <= sycl::linearize2(gid, 0, T/N, N) && sycl::linearize2(gid, 0, T/N, N) < T);
        loop_invariant \pointer(fx, T, 1\2);
        loop_invariant intermediateResults(T,N,sycl::h::warp_sizes(),fx,fxGs);

        loop_invariant (gid < T/N ) ==> result == sum(fxGs[0 .. sycl::linearize2(gid, 0, T/N, N)]);
        loop_invariant (gid == T/N ) ==> result == sum(fxGs[0 .. T]);

    */
    for (gid=0; gid < T/N; gid++){
        int lid = 0;
        //@ assert true;

        /*@ loop_invariant wrpSz == sycl::h::warp_sizes();
            loop_invariant 0 <= gid && gid < T/N;
            loop_invariant lid%wrpSz==0 && 0 <= lid && lid <= N;
            loop_invariant T%N==0 && N%wrpSz==0 && |fxGs| == T;
            loop_invariant \pointer(fx, T, 1\4);
            loop_invariant intermediateResults(T,N,sycl::h::warp_sizes(),fx,fxGs);
            loop_invariant (lid < N ) ==>  result == sum(fxGs[0 .. sycl::linearize2(gid, lid, T/N, N)]);
            loop_invariant (lid == N ) ==> result == sum(fxGs[0 .. sycl::linearize2(gid, N-wrpSz, T/N, N)+wrpSz]);
        */
        for (lid=0; lid < N; lid=lid+wrpSz){
            /*@ assert idshift(T,N,gid,lid,sycl::h::warp_sizes()) && modwrp(lid, wrpSz);
                assert intermediateResultsInstantiate(T,N,wrpSz,fx,fxGs,lid,gid);
                assert lidshift(lid,N,wrpSz);
                assert true; */
            result = result + fx[sycl::linearize2(gid, lid, T/N, N)];
            /*@ assert lemmaSumOverConcat(
                    fxGs[0 .. sycl::linearize2(gid, lid, T/N, N)],
                    fxGs[sycl::linearize2(gid, lid, T/N, N) .. sycl::linearize2(gid, lid, T/N, N) + wrpSz]
                );
                assert lemmaSumOverABBCisAC(
                    fxGs,
                    0,
                    sycl::linearize2(gid, lid, T/N, N),
                    sycl::linearize2(gid, lid, T/N, N),
                    sycl::linearize2(gid, lid, T/N, N)+wrpSz
                );
                assert true; */
        }
        //@ assert true;
    }
    return result;
}


