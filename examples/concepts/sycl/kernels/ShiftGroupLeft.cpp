#include <sycl/sycl.hpp>

/*@
  ensures \result > 0;
  pure int Tf();

  ensures \result > 0;
  pure int Nf();

  pure seq<int> fxGs();
*/

/*
- Check that delta is the same for all.
*/

/*@
  context_everywhere T == Tf() && N == Nf() && T > N && T%N == 0;
  context \pointer(fx, T, write);
@*/
void smartsum(sycl::queue q, int T, int N, int* fx) {
  //@ label bK;
  //@ assume |fxGs()| == Tf() && (\forall int i=0 .. Tf(); fxGs()[i] == \old[bK](fx[i]));
  //@ refute false;

  sycl::buffer<int, 1> fxBuf = sycl::buffer(fx, sycl::range<1>(T));
  //@ refute false;

  sycl::event e0 = q.submit([&](sycl::handler& h)
    {
    sycl::accessor<int, 1, sycl::access_mode::read_write> fxAcc = sycl::accessor(fxBuf, h, sycl::read_write);
    h.parallel_for(sycl::nd_range<1>(sycl::range<1>(T), sycl::range<1>(N)),
    /*@
        context Perm(fxAcc[it.get_global_id(0)], write);
        context (\forall int i=0 .. Tf(); fxGs()[i] == \old[bK](fx[i]));
        requires |fxGs()| == Tf() &&
          fxGs()[it.get_global_id(0)] == fxAcc[it.get_global_id(0)] &&
          fxGs()[it.get_global_id(0)] == \old(fx[it.get_global_id(0)]);
    */
      [=](sycl::nd_item<1> it)
        [[sycl::reqd_sub_group_size(32)]]
            {
        //@ refute false;
        sycl::sub_group sg = it.get_sub_group();
        int gid = it.get_global_id(0);
        int laneId = sg.get_local_id();
        int d1 = 1;

        //@ ghost int gsgid = 0;
        //@ ghost gsgid = gid+d1;
        //@ ghost gsgid = gsgid;
        //@ ghost gsgid = gsgid+gid+d2;
        //@ ghost gsgid = gsgid;

        fxAcc[gid] += sycl::shift_group_left(sg, fxAcc[gid], d1)
          /*@ sub_group_inv { \sg_val == sum(fxGs()[\gtid..\gtid+d1]) } */;

        /*@ assert lemmaSumOverConcat(fxGs()[gid .. gid+d1],fxGs()[gid+d1 .. gid+d1+d1]);
            assert lemmaSumOverABBCisAC(fxGs(), gid, gid+d1, gid+d1+d1);
            refute false; */

        int d2 = 2;
        fxAcc[gid] += sycl::shift_group_left(sg, fxAcc[gid], d2)
          /*@ sub_group_inv { \sg_val == sum(fxGs()[\gtid..\gtid+d2]) } */;

        /*@ assert lemmaSumOverConcat(fxGs()[gid .. gid+d2],fxGs()[gid+d2 .. gid+d2+d2]);
            assert lemmaSumOverABBCisAC(fxGs(), gid, gid+d2, gid+d2+d2);
            refute false;*/

        int dk = 4;
        //@ ghost int k1 = 2;

        /*@
        loop_invariant k1 >= 2 && dk == sycl::h::exp(2,k1);
        loop_invariant 4 <= dk && dk <= sg.get_local_range(0) && k1 <= logTwo(sg.get_local_range(0));
        loop_invariant dk == sg.get_local_range(0) ==> sycl::h::exp(2,k1) == sg.get_local_range(0);
        loop_invariant Perm({:fxAcc[it.get_global_id(0)]:}, write);

        loop_invariant (\forall int i=0 .. Tf(); fxGs()[i] == \old[bK](fx[i]));
        loop_invariant  sg.get_local_id() + dk <= sg.get_local_range(0) ==>
            fxAcc[gid] == sum(fxGs()[gid..gid+dk]);
        */
        for (int dk = 4; dk < sg.get_local_range(0); dk = dk * 2) {
          fxAcc[gid] += sycl::shift_group_left(sg, fxAcc[gid], dk)
            /*@ sub_group_inv { \sg_val == sum(fxGs()[\gtid..\gtid+dk]) } */;

          /*@ assert lemmaSumOverConcat(fxGs()[gid .. gid+dk],fxGs()[gid+dk .. gid+dk+dk]);
              assert lemmaSumOverABBCisAC(fxGs(), gid, gid+dk, gid+dk+dk);
              ghost k1=k1+1; */
        }
        //@ refute false;
    });
  });
  //@ refute false;
  e0.wait();
  //@ refute false;
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

/*
pure bool NBound(int N) =
    (N == 16 && sycl::h::exp(2,4) == 16) ||
    (N == 32 && sycl::h::exp(2,5) == 32) ||
    (N == 64 && sycl::h::exp(2,6) == 64);
*/
