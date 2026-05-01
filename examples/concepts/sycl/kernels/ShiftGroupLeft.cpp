#include <sycl/sycl.hpp>

/*@
    ensures \result > 0;
    pure int Tf();

    ensures \result > 0;
    pure int Nf();

*/

/*
- Check that delta is the same for all.
*/

/*@
    given seq<int> fxGs;
    context_everywhere T == Tf() && N == Nf() && T > N && T%N == 0 && N%sycl::h::warp_sizes()==0 && N > 0;
    context \pointer(fx, T, write);
    requires |fxGs| == Tf() && (\forall int i=0 .. Tf(); fxGs[i] == fx[i]);
    ensures |fxGs| == Tf() && (\forall int i=0 .. Tf(); fxGs[i] == \old(fx[i]));
    ensures \result == sum(fxGs[0 .. Tf()]);
@*/
int smartsum(sycl::queue q, int T, int N, int* fx) {
    //@ label bK;
    //@ refute false;
    {
        sycl::buffer<int, 1> fxBuf = sycl::buffer(fx, sycl::range<1>(T));
        //@ refute false;

        sycl::event e0 = q.submit([&](sycl::handler& h)
        {
            sycl::accessor<int, 1, sycl::access_mode::read_write> fxAcc = sycl::accessor(fxBuf, h, sycl::read_write);

            h.parallel_for(sycl::nd_range<1>(sycl::range<1>(T), sycl::range<1>(N)),
            /*@
            context Perm(fxAcc[it.get_global_id(0)], write);
            ensures fxAcc == \old(fxAcc);
            context (\forall int i=0 .. Tf(); fxGs[i] == \old[bK](fx[i]));
            requires |fxGs| == Tf() &&
                fxGs[it.get_global_id(0)] == fxAcc[it.get_global_id(0)] &&
                fxGs[it.get_global_id(0)] == \old(fx[it.get_global_id(0)]);

            ensures it.get_sub_group().get_local_id() + it.get_sub_group().get_local_range(0) <= it.get_sub_group().get_local_range(0) ==>
                it.get_global_id(0)+it.get_sub_group().get_local_range(0) <= |fxGs| ==>
                    fxAcc[it.get_global_id(0)] == sum(fxGs[it.get_global_id(0) .. it.get_global_id(0)+it.get_sub_group().get_local_range(0)]);
            */
            [=](sycl::nd_item<1> it) {
            // [=](sycl::nd_item<1> it) [[sycl::reqd_sub_group_size(32)]] {
                //@ refute false;
                sycl::sub_group sg = it.get_sub_group();
                int gid = it.get_global_id(0);
                int laneId = sg.get_local_id();
                int d1 = 1;

                fxAcc[gid] += sycl::shift_group_left(sg, fxAcc[gid], d1)
                /*@ sub_group_inv { \gtid+d1 <= |fxGs| ==> \sg_val == sum(fxGs[\gtid .. \gtid+d1]) } */;


                /*@ assert lemmaSumOverConcat(fxGs[gid .. gid+d1],fxGs[gid+d1 .. gid+d1+d1]);
                assert lemmaSumOverABBCisAC(fxGs, gid, gid+d1, gid+d1+d1);
                refute false; */

                int d2 = 2;
                //@ assert sg.get_local_id() + d1 < sg.get_local_range(0) ==> gid+d2 <= |fxGs| ==> fxAcc[gid] == sum(fxGs[gid .. gid+d2]);

                fxAcc[gid] += sycl::shift_group_left(sg, fxAcc[gid], d2)
                /*@ sub_group_inv { sg.get_local_id() + d1 < sg.get_local_range(0) ==> \gtid+d2 <= |fxGs| ==> \sg_val == sum(fxGs[\gtid .. \gtid+d2]) } */;

                /*@ assert lemmaSumOverConcat(fxGs[gid .. gid+d2],fxGs[gid+d2 .. gid+d2+d2]);
                assert lemmaSumOverABBCisAC(fxGs, gid, gid+d2, gid+d2+d2);
                refute false;*/

                /*@ assert sg.get_local_id() + d2 + d1 < sg.get_local_range(0) ==>
                        gid+d2+d2 <= |fxGs| ==> fxAcc[gid] == sum(fxGs[gid .. gid+d2+d2]);*/

                /*@ assert sg.get_local_id() + sycl::h::exp(2, 2) <= sg.get_local_range(0) ==>
                        gid+d2+d2 <= |fxGs| ==> fxAcc[gid] == sum(fxGs[gid .. gid+d2+d2]);*/

                int dk = 4;
                //@ ghost int k1 = 2;

                /*@ loop_invariant k1 >= 2 && dk == sycl::h::exp(2,k1);
                loop_invariant sg.get_local_range(0) %2==0;
                loop_invariant 4 <= dk && dk <= sg.get_local_range(0) && k1 <= logTwo(sg.get_local_range(0));
                loop_invariant dk == sg.get_local_range(0) ==> sycl::h::exp(2,k1) == sg.get_local_range(0);
                loop_invariant dk >= sg.get_local_range(0) ==> dk == sg.get_local_range(0);
                loop_invariant Perm({:fxAcc[it.get_global_id(0)]:}, write);

                loop_invariant (\forall int i=0 .. Tf(); fxGs[i] == \old[bK](fx[i]));
                loop_invariant  sg.get_local_id() + sycl::h::exp(2,k1) <= sg.get_local_range(0) ==>
                    gid+dk <= |fxGs| ==> fxAcc[gid] == sum(fxGs[gid .. gid+dk]); */
                for (dk = 4; dk < sg.get_local_range(0); dk = dk * 2) {
                    int sgl_result2 = sycl::shift_group_left(sg, fxAcc[gid], dk)
                    /*@ sub_group_inv { sg.get_local_id() + dk <= sg.get_local_range(0) ==> \gtid+dk <= |fxGs| ==> \sg_val == sum(fxGs[\gtid .. \gtid+dk]) } */;
                    fxAcc[gid] += sgl_result2;

                    /*@ assert lemmaSumOverConcat(fxGs[gid .. gid+dk],fxGs[gid+dk .. gid+dk+dk]);
                    assert lemmaSumOverABBCisAC(fxGs, gid, gid+dk, gid+dk+dk);
                    refute false;*/

                    /*@ ghost k1=k1+1; */
                }
                //@ refute false;
            });});
        //@ refute false;
        e0.wait();
        //@ refute false;
    }

    int result = accumulateResult(T,N,fx) 
        /*@ given {fxGs=fxGs} */;


    //@ refute false;
    //@ assert result == sum(fxGs[0 .. Tf()]);
    return result;
}

/*@
    given seq<int> fxGs;
    context_everywhere T == Tf() && N == Nf() && T > N && T%N == 0 && N%sycl::h::warp_sizes()==0 && N > 0;
    context_everywhere |fxGs| == Tf();
    context \pointer(fx, T, 1\2);

    context (\forall int lid1=0 .. N, int gid1=0 .. T/N;
                lid1 % sycl::h::warp_sizes() + sycl::h::warp_sizes() <= sycl::h::warp_sizes() ==> sycl::linearize2(gid1, lid1, T/N, N) + sycl::h::warp_sizes() <= |fxGs| ==>
                    fx[{:sycl::linearize2(gid1, lid1, T/N, N):}] == sum(fxGs[sycl::linearize2(gid1, lid1, T/N, N) .. sycl::linearize2(gid1, lid1, T/N, N) + sycl::h::warp_sizes()]));


    ensures \result == sum(fxGs[0 .. Tf()]);
@*/
int accumulateResult(int T, int N, int* fx) {
    //@ ghost int wrpSz = sycl::h::warp_sizes();
    /*@
    assert (\forall int lid=0 .. N, int gid=0 .. T/N;
        lid % wrpSz + wrpSz <= wrpSz ==>
        sycl::linearize2(gid, lid, T/N, N) + wrpSz <= |fxGs| ==>
              fx[{:sycl::linearize2(gid, lid, T/N, N):}] == sum(fxGs[sycl::linearize2(gid, lid, T/N, N) .. sycl::linearize2(gid, lid, T/N, N) + wrpSz])
    );
    */

    int result = 0;
    int gid = 0;
    //@ refute false;

    /*@ loop_invariant wrpSz == sycl::h::warp_sizes();
        loop_invariant T == Tf() && N == Nf() && T > N && T%N == 0 && N%wrpSz==0;
        loop_invariant 0 <= gid && gid <= T/N;
        loop_invariant T%N==0 && N%wrpSz==0 && |fxGs| == Tf();
        loop_invariant (gid < T/N ) ==> (0 <= sycl::linearize2(gid, 0, T/N, N) && sycl::linearize2(gid, 0, T/N, N) < Tf());
        loop_invariant \pointer(fx, T, 1\2);
        loop_invariant (\forall int lid1=0 .. N, int gid1=0 .. T/N;
                        lid1 % wrpSz + wrpSz <= wrpSz ==> sycl::linearize2(gid1, lid1, T/N, N) + wrpSz <= |fxGs| ==>
                            fx[{:sycl::linearize2(gid1, lid1, T/N, N):}] == sum(fxGs[sycl::linearize2(gid1, lid1, T/N, N) .. sycl::linearize2(gid1, lid1, T/N, N) + wrpSz]));
        loop_invariant (gid < T/N ) ==> result == sum(fxGs[0 .. sycl::linearize2(gid, 0, T/N, N)]);
        loop_invariant (gid == T/N ) ==> result == sum(fxGs[0 .. Tf()]);

    */
    for (gid=0; gid < T/N; gid++){
        int lid = 0;
        //@ refute false;

        /*@ loop_invariant wrpSz == sycl::h::warp_sizes();
            loop_invariant 0 <= gid && gid < T/N;
            loop_invariant 0 <= lid && lid <= N && lid%wrpSz==0;
            loop_invariant T%N==0 && N%wrpSz==0 && |fxGs| == Tf();
            loop_invariant \pointer(fx, T, 1\4);
            loop_invariant (\forall int lid1=0 .. N, int gid1=0 .. T/N;
                lid1 % wrpSz + wrpSz <= wrpSz ==> sycl::linearize2(gid1, lid1, T/N, N) + wrpSz <= |fxGs| ==>
                    fx[{:sycl::linearize2(gid1, lid1, T/N, N):}] == sum(fxGs[sycl::linearize2(gid1, lid1, T/N, N) .. sycl::linearize2(gid1, lid1, T/N, N) + wrpSz]));

            loop_invariant (lid < N ) ==>  result == sum(fxGs[0 .. sycl::linearize2(gid, lid, T/N, N)]);
            loop_invariant (lid == N ) ==> result == sum(fxGs[0 .. sycl::linearize2(gid, N-wrpSz, T/N, N)+wrpSz]);
        */
        for (lid=0; lid < N; lid=lid+wrpSz){
            /*@
                assert sycl::linearize2(gid, lid, T/N, N) < T;
                assert fx[sycl::linearize2(gid, lid, T/N, N)] == sum(fxGs[sycl::linearize2(gid, lid, T/N, N) .. sycl::linearize2(gid, lid, T/N, N) + wrpSz]);
                assert result == sum(fxGs[0 .. sycl::linearize2(gid, lid, T/N, N)]);
                ghost int resultBU = result;
                assert resultBU == sum(fxGs[0 .. sycl::linearize2(gid, lid, T/N, N)]);
                refute false;

                */
            result = result + fx[sycl::linearize2(gid, lid, T/N, N)];
            /*@
                assert result == resultBU + fx[{:sycl::linearize2(gid, lid, T/N, N):}];
                assert result == sum(fxGs[0 .. sycl::linearize2(gid, lid, T/N, N)]) + fx[sycl::linearize2(gid, lid, T/N, N)];
                assert result == sum(fxGs[0 .. sycl::linearize2(gid, lid, T/N, N)]) + sum(fxGs[sycl::linearize2(gid, lid, T/N, N) .. sycl::linearize2(gid, lid, T/N, N) + wrpSz]);
                assert lemmaSumOverConcat(
                    fxGs[0 .. sycl::linearize2(gid, lid, T/N, N)],
                    fxGs[sycl::linearize2(gid, lid, T/N, N) .. sycl::linearize2(gid, lid, T/N, N) + wrpSz]
                );

                assert lemmaSumOverABBCisAC(
                    fxGs,
                    0,
                    sycl::linearize2(gid, lid, T/N, N),
                    sycl::linearize2(gid, lid, T/N, N)+wrpSz
                );
                assert result == sum(fxGs[0 ..  sycl::linearize2(gid, lid, T/N, N) + wrpSz]);
            */


            //  I have
            //     assert result == sum(fxGs[0 .. sycl::linearize2(gid, lid, T/N, N)]);
            //  I do an update and I have
            //@     assert result == sum(fxGs[0 ..  sycl::linearize2(gid, lid, T/N, N) + wrpSz]);
            //  Now when we update lid, we want to prove
            //     assert result == sum(fxGs[0 ..  sycl::linearize2(gid, lid+wrpSz, T/N, N)]);
            //  Which holds because,
            //@     ghost int lid2=lid+wrpSz;
            //@     assert (lid2 < N) ==> sycl::linearize2(gid, lid, T/N, N) + wrpSz == sycl::linearize2(gid, lid2, T/N, N);
            //@     assert (lid2 < N ) ==>  result == sum(fxGs[0 .. sycl::linearize2(gid, lid2, T/N, N)]);
            //  But if lid2 == N, then
            //@     assert (lid2 == N) ==> sycl::linearize2(gid, lid, T/N, N) + wrpSz == sycl::linearize2(gid, lid2-wrpSz, T/N, N)+wrpSz;
            //@     assert (lid2 == N) ==> result == sum(fxGs[0 .. sycl::linearize2(gid, lid2-wrpSz, T/N, N)+wrpSz]);
            // @ assert (lid2 < N) ==> (lid2%wrpSz==0  && 0<= lid2 && lid2 <=N);
            // @ assert (lid2 == N) ==> (lid2%wrpSz==0 && 0<= lid2 && lid2 <=N);
            //@ refute false;
        }
    }
    return result;
}


/////////////////////////////////////////
/// Sum related functions and lemma's ///
/////////////////////////////////////////
/*@
ensures |xs| == 0 ==> \result == 0;
ensures |xs| == 1 ==> \result == xs[0];
pure int sum(seq<int> xs) =
    0 < |xs| ? xs[0] + sum(xs[1 .. ]) : 0;

requires |xs| >= 0;
requires |ys| >= 0;
ensures \result;
ensures |xs| == 0 ==> sum(xs + ys) == sum(ys);
ensures |ys| == 0 ==> sum(xs + ys) == sum(xs);
ensures |xs + ys| == |xs| + |ys|;
ensures sum(xs[1 .. ] + ys) == sum(xs[1 .. ]) + sum(ys);
ensures sum(xs) + sum(ys) == sum(xs + ys);
pure bool lemmaSumOverConcat(seq<int> xs, seq<int> ys) =
    0 < |xs| ?
        lemmaSumOverConcat(xs[1 .. ], ys) &&
        xs[1 .. ] + ys == ((xs + ys)[1 .. ])
        :
        true;

requires a <= b && b <= c;
ensures \result;
pure bool lemmaSumOverABBCisAC(seq<int> xs, int a, int b, int c) = xs[a .. c] == xs[a .. b] + xs[b .. c];

ensures \result >= 0;
ensures N == 64 ==> \result == 6;
ensures N == 32 ==> \result == 5;
ensures N == 16 ==> \result == 4;
ensures sycl::h::exp(2,\result) == N;
pure int logTwo(int N);
*/
















