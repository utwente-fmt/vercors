#include <sycl/sycl.hpp>

/*@
    ensures \result > 0;
    pure int Tf();

    ensures \result > 0;
    pure int Nf();

    pure seq<int> fxGs();
    pure seq<int> fyGs();
    pure seq<int> fzGs();
*/

/*
- Check that delta is the same for all.
*/

/*@
    context_everywhere T == Tf() && N == Nf() && T > N && T%N == 0 && N%32==0 && N > 0;
    context \pointer(fx, T, write) ** \pointer(fy, T, write) ** \pointer(fz, T, write);
    ensures |fxGs()| == Tf() && (\forall int i=0 .. Tf(); fxGs()[i] == \old(fx[i]));
    ensures |fyGs()| == Tf() && (\forall int i=0 .. Tf(); fyGs()[i] == \old(fy[i]));
    ensures |fzGs()| == Tf() && (\forall int i=0 .. Tf(); fzGs()[i] == \old(fz[i]));

    ensures \result == sum(fxGs()[0 .. Tf()]) + sum(fyGs()[0 .. Tf()]) + sum(fzGs()[0 .. Tf()]);
@*/
int smartsum(sycl::queue q, int T, int N, int* fx, int* fy, int* fz) {
    //@ label bK;
    //@ assume |fxGs()| == Tf() && (\forall int i=0 .. Tf(); fxGs()[i] == \old[bK](fx[i]));
    //@ assume |fyGs()| == Tf() && (\forall int i=0 .. Tf(); fyGs()[i] == \old[bK](fy[i]));
    //@ assume |fzGs()| == Tf() && (\forall int i=0 .. Tf(); fzGs()[i] == \old[bK](fz[i]));
    //@ assert true;
    {
        sycl::buffer<int, 1> fxBuf = sycl::buffer(fx, sycl::range<1>(T));
        sycl::buffer<int, 1> fyBuf = sycl::buffer(fy, sycl::range<1>(T));
        sycl::buffer<int, 1> fzBuf = sycl::buffer(fz, sycl::range<1>(T));

        //@ assert true;

        sycl::event e0 = q.submit([&](sycl::handler& h)
        {
            sycl::accessor<int, 1, sycl::access_mode::read_write> fxAcc = sycl::accessor(fxBuf, h, sycl::read_write);
            sycl::accessor<int, 1, sycl::access_mode::read_write> fyAcc = sycl::accessor(fyBuf, h, sycl::read_write);
            sycl::accessor<int, 1, sycl::access_mode::read_write> fzAcc = sycl::accessor(fzBuf, h, sycl::read_write);

            h.parallel_for(sycl::nd_range<1>(sycl::range<1>(T), sycl::range<1>(N)),
            /*@
            context Perm(fxAcc[it.get_global_id(0)], write) ** Perm(fyAcc[it.get_global_id(0)], write) ** Perm(fzAcc[it.get_global_id(0)], write);
            ensures fxAcc == \old(fxAcc) ** fyAcc == \old(fyAcc) ** fzAcc == \old(fzAcc);
            context (\forall int i=0 .. Tf(); fxGs()[i] == \old[bK](fx[i]) && fyGs()[i] == \old[bK](fy[i]) && fzGs()[i] == \old[bK](fz[i]));
            requires |fxGs()| == Tf() && fxGs()[it.get_global_id(0)] == fxAcc[it.get_global_id(0)] && fxGs()[it.get_global_id(0)] == \old(fx[it.get_global_id(0)]);
            requires |fyGs()| == Tf() && fyGs()[it.get_global_id(0)] == fyAcc[it.get_global_id(0)] && fyGs()[it.get_global_id(0)] == \old(fy[it.get_global_id(0)]);
            requires |fzGs()| == Tf() && fzGs()[it.get_global_id(0)] == fzAcc[it.get_global_id(0)] && fzGs()[it.get_global_id(0)] == \old(fz[it.get_global_id(0)]);

            ensures it.get_global_id(0)%4 == 0 ==>
                (it.get_sub_group().get_local_id()+it.get_sub_group().get_local_range(0)   <= it.get_sub_group().get_local_range(0) ==>
                it.get_global_id(0)+it.get_sub_group().get_local_range(0)   <= |fxGs()| ==>
                    fxAcc[it.get_global_id(0)] == sum(fxGs()[it.get_global_id(0)   .. it.get_global_id(0)+it.get_sub_group().get_local_range(0)]));
            ensures it.get_global_id(0)%4 == 1 ==> (0 <= it.get_sub_group().get_local_id() - 1 ==>
                it.get_sub_group().get_local_id()+it.get_sub_group().get_local_range(0)-1 <= it.get_sub_group().get_local_range(0) ==>
                it.get_global_id(0)+it.get_sub_group().get_local_range(0)-1 <= |fyGs()| ==>
                    fxAcc[it.get_global_id(0)] == sum(fyGs()[it.get_global_id(0)-1 .. it.get_global_id(0)+it.get_sub_group().get_local_range(0)-1]));
            ensures it.get_global_id(0)%4 == 2 ==> (0 <= it.get_sub_group().get_local_id() - 2 ==>
                it.get_sub_group().get_local_id()+it.get_sub_group().get_local_range(0)-2 <= it.get_sub_group().get_local_range(0) ==>
                it.get_global_id(0)+it.get_sub_group().get_local_range(0)-2 <= |fzGs()| ==>
                    fxAcc[it.get_global_id(0)] == sum(fzGs()[it.get_global_id(0)-2 .. it.get_global_id(0)+it.get_sub_group().get_local_range(0)-2]));
            */
            [=](sycl::nd_item<1> it) [[sycl::reqd_sub_group_size(32)]] {
                //@ assert true;
                sycl::sub_group sg = it.get_sub_group();
                int gid = it.get_global_id(0);
                int laneId = sg.get_local_id();
                int wrpsz = sg.get_local_range(0);
                int d1 = 1;

                fxAcc[gid] += sycl::shift_group_left(sg, fxAcc[gid], d1)
                /*@ sub_group_inv { \gtid+d1 <= |fxGs()| ==> \sg_val == sum(fxGs()[\gtid .. \gtid+d1]) } */;
                fyAcc[gid] += sycl::shift_group_right(sg, fyAcc[gid], d1)
                /*@ sub_group_inv { \gtid+d1 <= |fyGs()| ==> \sg_val == sum(fyGs()[\gtid .. \gtid+d1]) } */;
                fzAcc[gid] += sycl::shift_group_left(sg, fzAcc[gid], d1)
                /*@ sub_group_inv { \gtid+d1 <= |fzGs()| ==> \sg_val == sum(fzGs()[\gtid .. \gtid+d1]) } */;


                /*@ assert lemmaSumOverConcat(fxGs()[gid    .. gid+d1],  fxGs()[gid+d1 .. gid+d1+d1]) && lemmaSumOverABBCisAC(fxGs(), gid,    gid+d1,   gid+d1, gid+d1+d1);
                    assert lemmaSumOverConcat(fzGs()[gid    .. gid+d1],  fzGs()[gid+d1 .. gid+d1+d1]) && lemmaSumOverABBCisAC(fzGs(), gid,    gid+d1,   gid+d1, gid+d1+d1);
                    assert lemmaSumOverConcat(fyGs()[gid-d1 .. gid-d1+1],fyGs()[gid    .. gid+1])     && lemmaSumOverABBCisAC(fyGs(), gid-d1, gid-d1+1, gid, gid+1);
                    assert true; */

                if (gid % 2 != 0) { fxAcc[gid] = fyAcc[gid]; }

                int d2 = 2;
                fxAcc[gid] += sycl::shift_group_left(sg, fxAcc[gid], d2)
                /*@ sub_group_inv { (\gtid % 2 == 0) ?
                    (\sgtid + d1 < wrpsz ==> \gtid+d2 <= |fzGs()| ==> \sg_val == sum(fxGs()[\gtid .. \gtid+d2])) :
                    (0 <= \sgtid - d1 ==> \sg_val == sum(fyGs()[\gtid-d1 .. \gtid+1])) } */;

                fzAcc[gid] += sycl::shift_group_right(sg, fzAcc[gid], d2)
                /*@ sub_group_inv { \sgtid+d1 < wrpsz ==> \gtid+d2 <= |fzGs()| ==> \sg_val == sum(fzGs()[\gtid .. \gtid+d2]) } */;

                /*@ assert lemmaSumOverConcat(fxGs()[gid    .. gid+d2],   fxGs()[gid+d2 .. gid+d2+d2])  && lemmaSumOverABBCisAC(fxGs(), gid,    gid+d2, gid+d2, gid+d2+d2);
                    assert lemmaSumOverConcat(fyGs()[gid-d1 .. gid+1],    fyGs()[gid+1  .. gid+d2+1])   && lemmaSumOverABBCisAC(fyGs(), gid-d1, gid+1,  gid+d2-d1, gid+d2+1);
                    assert lemmaSumOverConcat(fzGs()[gid-d2 .. gid-d2+d2],fzGs()[gid    .. gid+d2])     && lemmaSumOverABBCisAC(fzGs(), gid-d2, gid,    gid, gid+d2);
                    assert true; */

                if (gid % 4 >= 2) { fxAcc[gid] = fzAcc[gid]; }


               int dk = 4;
                //@ ghost int k1 = 2;

                /*@ loop_invariant k1 >= 2 && dk == sycl::h::exp(2,k1);
                loop_invariant wrpsz % 2==0;
                loop_invariant 4 <= dk && dk <= wrpsz && k1 <= logTwo(wrpsz);
                loop_invariant dk == wrpsz ==> sycl::h::exp(2,k1) == wrpsz;
                loop_invariant dk >= wrpsz ==> dk == wrpsz;
                loop_invariant Perm({:fxAcc[gid]:}, write);

                loop_invariant (\forall int i=0 .. Tf(); fxGs()[i] == \old[bK](fx[i]));
                loop_invariant (\forall int i=0 .. Tf(); fyGs()[i] == \old[bK](fy[i]));
                loop_invariant (\forall int i=0 .. Tf(); fzGs()[i] == \old[bK](fz[i]));
                loop_invariant |fzGs()| == |fyGs()| && |fxGs()| == |fyGs()| && |fzGs()| == Tf();

                loop_invariant gid%4 == 0 ==>                     (laneId+dk   <= wrpsz ==> gid+dk   <= |fxGs()| ==> fxAcc[gid] == sum(fxGs()[gid   .. gid+dk]));
                loop_invariant gid%4 == 1 ==> (0 <= laneId - 1 ==> laneId+dk-1 <= wrpsz ==> gid+dk-1 <= |fyGs()| ==> fxAcc[gid] == sum(fyGs()[gid-1 .. gid+dk-1]));
                loop_invariant gid%4 == 2 ==> (0 <= laneId - 2 ==> laneId+dk-2 <= wrpsz ==> gid+dk-2 <= |fzGs()| ==> fxAcc[gid] == sum(fzGs()[gid-2 .. gid+dk-2]));
                */
                for (dk = 4; dk < wrpsz; dk = dk * 2) {
                    int sgl_result2 = sycl::shift_group_left(sg, fxAcc[gid], dk)
                        /*@ sub_group_inv {   (\gtid%4 == 0) ?                     (\sgtid+dk   <= wrpsz ==> \gtid + dk <= |fxGs()| ==> \sg_val == sum(fxGs()[\gtid   .. \gtid+dk]))
                                            : (\gtid%4 == 1) ? (0 <= \sgtid - 1 ==> \sgtid+dk-1 <= wrpsz ==> \gtid+dk-1 <= |fyGs()| ==> \sg_val == sum(fyGs()[\gtid-1 .. \gtid+dk-1]))
                                            : (\gtid%4 == 2) ? (0 <= \sgtid - 2 ==> \sgtid+dk-2 <= wrpsz ==> \gtid+dk-2 <= |fzGs()| ==> \sg_val == sum(fzGs()[\gtid-2 .. \gtid+dk-2]))
                                            : true } */;
                        fxAcc[gid] += sgl_result2;

                        /*@ assert lemmaSumOverConcat(fxGs()[gid   .. gid+dk  ],fxGs()[gid+dk   .. gid+dk+dk  ]) && lemmaSumOverABBCisAC(fxGs(), gid  , gid+dk  , gid+dk  , gid+dk+dk  );
                            assert lemmaSumOverConcat(fyGs()[gid-1 .. gid+dk-1],fyGs()[gid+dk-1 .. gid+dk+dk-1]) && lemmaSumOverABBCisAC(fyGs(), gid-1, gid+dk-1, gid+dk-1, gid+dk+dk-1);
                            assert lemmaSumOverConcat(fzGs()[gid-2 .. gid+dk-2],fzGs()[gid+dk-2 .. gid+dk+dk-2]) && lemmaSumOverABBCisAC(fzGs(), gid-2, gid+dk-2, gid+dk-2, gid+dk+dk-2);
                            assert true;*/

                        /*@ ghost k1=k1+1; */
                }

                /*@ assert gid%4 == 0 ==>                     (laneId+wrpsz   <= wrpsz ==> gid+wrpsz   <= |fxGs()| ==> fxAcc[gid] == sum(fxGs()[gid   .. gid+wrpsz]));
                    assert gid%4 == 1 ==> (0 <= laneId - 1 ==> laneId+wrpsz-1 <= wrpsz ==> gid+wrpsz-1 <= |fyGs()| ==> fxAcc[gid] == sum(fyGs()[gid-1 .. gid+wrpsz-1]));
                    assert gid%4 == 2 ==> (0 <= laneId - 2 ==> laneId+wrpsz-2 <= wrpsz ==> gid+wrpsz-2 <= |fzGs()| ==> fxAcc[gid] == sum(fzGs()[gid-2 .. gid+wrpsz-2]));
                    assert true; */
            });});
        //@ assert true;
        e0.wait();
        //@ assert true;
    }

    return 0;
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

requires a <= b && c <= d && b == c;
ensures \result;
pure bool lemmaSumOverABBCisAC(seq<int> xs, int a, int b, int c, int d) = xs[a .. d] == xs[a .. b] + xs[c .. d];

ensures \result >= 0;
ensures N == 64 ==> \result == 6;
ensures N == 32 ==> \result == 5;
ensures N == 16 ==> \result == 4;
ensures sycl::h::exp(2,\result) == N;
pure int logTwo(int N);
*/

/*
pure bool NBound(int N) =
    (N == 16 && sycl::h::exp(2,4) == 16) ||
    (N == 32 && sycl::h::exp(2,5) == 32) ||
    (N == 64 && sycl::h::exp(2,6) == 64);
*/