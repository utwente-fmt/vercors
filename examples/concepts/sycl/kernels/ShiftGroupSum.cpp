

#include <sycl/sycl.hpp>

/*@ ensures \result > 0;
    pure int Tf();

    ensures \result > 0;
    pure int Nf(); */

/*
- Check that delta is the same for all.
*/

/*@
    given seq<int> fxGs;
    given seq<int> fyGs;
    given seq<int> fzGs;
    yields int resultxx;
    yields int resultyy;
    yields int resultzz;
    context_everywhere T == Tf() && N == Nf() && T > N && T%N == 0 && N%32==0 && N > 0;
    context \pointer(fx, T, write) ** \pointer(fy, T, write) ** \pointer(fz, T, write);
    requires |fxGs| == Tf() && (\forall int i=0 .. Tf(); fxGs[i] == fx[i]);
    requires |fyGs| == Tf() && (\forall int i=0 .. Tf(); fyGs[i] == fy[i]);
    requires |fzGs| == Tf() && (\forall int i=0 .. Tf(); fzGs[i] == fz[i]);

    ensures |fxGs| == Tf() && (\forall int i=0 .. Tf(); fxGs[i] == \old(fx[i]));
    ensures |fyGs| == Tf() && (\forall int i=0 .. Tf(); fyGs[i] == \old(fy[i]));
    ensures |fzGs| == Tf() && (\forall int i=0 .. Tf(); fzGs[i] == \old(fz[i]));

    ensures resultxx == sum(fxGs[0 .. Tf()]);
    ensures resultyy == sum(fyGs[0 .. Tf()]);
    ensures resultzz == sum(fzGs[0 .. Tf()]);
@*/
void smartsum(sycl::queue q, int T, int N, int* fx, int* fy, int* fz) {
    //@ label bK;
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
            /*@ context Perm(fxAcc[it.get_global_id(0)], write) ** Perm(fyAcc[it.get_global_id(0)], write) ** Perm(fzAcc[it.get_global_id(0)], write);
                ensures fxAcc == \old(fxAcc) ** fyAcc == \old(fyAcc) ** fzAcc == \old(fzAcc);
                context (\forall int i=0 .. Tf(); fxGs[i] == \old[bK](fx[i]) && fyGs[i] == \old[bK](fy[i]) && fzGs[i] == \old[bK](fz[i]));
                requires |fxGs| == Tf() && fxGs[it.get_global_id(0)] == fxAcc[it.get_global_id(0)] && fxGs[it.get_global_id(0)] == \old(fx[it.get_global_id(0)]);
                requires |fyGs| == Tf() && fyGs[it.get_global_id(0)] == fyAcc[it.get_global_id(0)] && fyGs[it.get_global_id(0)] == \old(fy[it.get_global_id(0)]);
                requires |fzGs| == Tf() && fzGs[it.get_global_id(0)] == fzAcc[it.get_global_id(0)] && fzGs[it.get_global_id(0)] == \old(fz[it.get_global_id(0)]);

                ensures it.get_global_id(0)%4 == 0 ==>
                    (it.get_sub_group().get_local_id()+it.get_sub_group().get_local_range(0)   <= it.get_sub_group().get_local_range(0) ==>
                    it.get_global_id(0)+it.get_sub_group().get_local_range(0)   <= |fxGs| ==>
                        fxAcc[it.get_global_id(0)] == sum(fxGs[it.get_global_id(0)   .. it.get_global_id(0)+it.get_sub_group().get_local_range(0)]));
                ensures it.get_global_id(0)%4 == 1 ==> (0 <= it.get_sub_group().get_local_id() - 1 ==>
                    it.get_sub_group().get_local_id()+it.get_sub_group().get_local_range(0)-1 <= it.get_sub_group().get_local_range(0) ==>
                    it.get_global_id(0)+it.get_sub_group().get_local_range(0)-1 <= |fyGs| ==>
                        fxAcc[it.get_global_id(0)] == sum(fyGs[it.get_global_id(0)-1 .. it.get_global_id(0)+it.get_sub_group().get_local_range(0)-1]));
                ensures it.get_global_id(0)%4 == 2 ==> (0 <= it.get_sub_group().get_local_id() - 2 ==>
                    it.get_sub_group().get_local_id()+it.get_sub_group().get_local_range(0)-2 <= it.get_sub_group().get_local_range(0) ==>
                    it.get_global_id(0)+it.get_sub_group().get_local_range(0)-2 <= |fzGs| ==>
                        fxAcc[it.get_global_id(0)] == sum(fzGs[it.get_global_id(0)-2 .. it.get_global_id(0)+it.get_sub_group().get_local_range(0)-2])); */
            [=](sycl::nd_item<1> it) [[sycl::reqd_sub_group_size(32)]] {
                    //@ assert true;
                    sycl::sub_group sg = it.get_sub_group();
                    int gid = it.get_global_id(0);
                    int laneId = sg.get_local_id();
                    int wrpsz = sg.get_local_range(0);
                    int d1 = 1;

                    fxAcc[gid] += sycl::shift_group_left(sg, fxAcc[gid], d1)
                    /*@ sub_group_inv { \gtid+d1 <= |fxGs| ==> \sg_val == sum(fxGs[\gtid .. \gtid+d1]) } */;
                    fyAcc[gid] += sycl::shift_group_right(sg, fyAcc[gid], d1)
                    /*@ sub_group_inv { \gtid+d1 <= |fyGs| ==> \sg_val == sum(fyGs[\gtid .. \gtid+d1]) } */;
                    fzAcc[gid] += sycl::shift_group_left(sg, fzAcc[gid], d1)
                    /*@ sub_group_inv { \gtid+d1 <= |fzGs| ==> \sg_val == sum(fzGs[\gtid .. \gtid+d1]) } */;


                    /*@ assert lemmaSumOverConcat(fxGs[gid    .. gid+d1],  fxGs[gid+d1 .. gid+d1+d1]) && lemmaSumOverABBCisAC(fxGs, gid,    gid+d1,   gid+d1, gid+d1+d1);
                        assert lemmaSumOverConcat(fzGs[gid    .. gid+d1],  fzGs[gid+d1 .. gid+d1+d1]) && lemmaSumOverABBCisAC(fzGs, gid,    gid+d1,   gid+d1, gid+d1+d1);
                        assert lemmaSumOverConcat(fyGs[gid-d1 .. gid-d1+1],fyGs[gid    .. gid+1])     && lemmaSumOverABBCisAC(fyGs, gid-d1, gid-d1+1, gid, gid+1);
                        assert true; */

                    if (gid % 2 != 0) { fxAcc[gid] = fyAcc[gid]; }

                    int d2 = 2;
                    fxAcc[gid] += sycl::shift_group_left(sg, fxAcc[gid], d2)
                    /*@ sub_group_inv { (\gtid % 2 == 0) ?
                        (\sgtid + d1 < wrpsz ==> \gtid+d2 <= |fzGs| ==> \sg_val == sum(fxGs[\gtid .. \gtid+d2])) :
                        (0 <= \sgtid - d1 ==> \sg_val == sum(fyGs[\gtid-d1 .. \gtid+1])) } */;

                    fzAcc[gid] += sycl::shift_group_right(sg, fzAcc[gid], d2)
                    /*@ sub_group_inv { \sgtid+d1 < wrpsz ==> \gtid+d2 <= |fzGs| ==> \sg_val == sum(fzGs[\gtid .. \gtid+d2]) } */;

                    /*@ assert lemmaSumOverConcat(fxGs[gid    .. gid+d2],   fxGs[gid+d2 .. gid+d2+d2])  && lemmaSumOverABBCisAC(fxGs, gid,    gid+d2, gid+d2, gid+d2+d2);
                        assert lemmaSumOverConcat(fyGs[gid-d1 .. gid+1],    fyGs[gid+1  .. gid+d2+1])   && lemmaSumOverABBCisAC(fyGs, gid-d1, gid+1,  gid+d2-d1, gid+d2+1);
                        assert lemmaSumOverConcat(fzGs[gid-d2 .. gid-d2+d2],fzGs[gid    .. gid+d2])     && lemmaSumOverABBCisAC(fzGs, gid-d2, gid,    gid, gid+d2);
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

                    loop_invariant (\forall int i=0 .. Tf(); fxGs[i] == \old[bK](fx[i]));
                    loop_invariant (\forall int i=0 .. Tf(); fyGs[i] == \old[bK](fy[i]));
                    loop_invariant (\forall int i=0 .. Tf(); fzGs[i] == \old[bK](fz[i]));
                    loop_invariant |fzGs| == |fyGs| && |fxGs| == |fyGs| && |fzGs| == Tf();

                    loop_invariant gid%4 == 0 ==>                     (laneId+dk   <= wrpsz ==> gid+dk   <= |fxGs| ==> fxAcc[gid] == sum(fxGs[gid   .. gid+dk]));
                    loop_invariant gid%4 == 1 ==> (0 <= laneId - 1 ==> laneId+dk-1 <= wrpsz ==> gid+dk-1 <= |fyGs| ==> fxAcc[gid] == sum(fyGs[gid-1 .. gid+dk-1]));
                    loop_invariant gid%4 == 2 ==> (0 <= laneId - 2 ==> laneId+dk-2 <= wrpsz ==> gid+dk-2 <= |fzGs| ==> fxAcc[gid] == sum(fzGs[gid-2 .. gid+dk-2]));
                    */
                    for (dk = 4; dk < wrpsz; dk = dk * 2) {
                        int sgl_result2 = sycl::shift_group_left(sg, fxAcc[gid], dk)
                            /*@ sub_group_inv {   (\gtid%4 == 0) ?                     (\sgtid+dk   <= wrpsz ==> \gtid + dk <= |fxGs| ==> \sg_val == sum(fxGs[\gtid   .. \gtid+dk]))
                                                : (\gtid%4 == 1) ? (0 <= \sgtid - 1 ==> \sgtid+dk-1 <= wrpsz ==> \gtid+dk-1 <= |fyGs| ==> \sg_val == sum(fyGs[\gtid-1 .. \gtid+dk-1]))
                                                : (\gtid%4 == 2) ? (0 <= \sgtid - 2 ==> \sgtid+dk-2 <= wrpsz ==> \gtid+dk-2 <= |fzGs| ==> \sg_val == sum(fzGs[\gtid-2 .. \gtid+dk-2]))
                                                : true } */;
                            fxAcc[gid] += sgl_result2;

                            /*@ assert lemmaSumOverConcat(fxGs[gid   .. gid+dk  ],fxGs[gid+dk   .. gid+dk+dk  ]) && lemmaSumOverABBCisAC(fxGs, gid  , gid+dk  , gid+dk  , gid+dk+dk  );
                                assert lemmaSumOverConcat(fyGs[gid-1 .. gid+dk-1],fyGs[gid+dk-1 .. gid+dk+dk-1]) && lemmaSumOverABBCisAC(fyGs, gid-1, gid+dk-1, gid+dk-1, gid+dk+dk-1);
                                assert lemmaSumOverConcat(fzGs[gid-2 .. gid+dk-2],fzGs[gid+dk-2 .. gid+dk+dk-2]) && lemmaSumOverABBCisAC(fzGs, gid-2, gid+dk-2, gid+dk-2, gid+dk+dk-2);
                                assert true;*/

                            /*@ ghost k1=k1+1; */
                    }

            });});
        //@ assert true;
        e0.wait();
        //@ assert true;
    }

    accumulateResult(T,N,fx) 
        /*@ given {fxGs=fxGs, fyGs=fyGs, fzGs=fzGs}*/
        /*@ yields { resultxx = resultxx, resultyy = resultyy, resultzz = resultzz  } */;
}


/*@
    given seq<int> fxGs;
    given seq<int> fyGs;
    given seq<int> fzGs;
    yields int resultxx;
    yields int resultyy;
    yields int resultzz;
    context_everywhere T == Tf() && N == Nf() && T > N && T%N == 0 && N%32==0 && N > 0;
    context_everywhere \pointer(fx, T, 1\2);
    context_everywhere (\forall int lid1=0 .. N, int gid1=0 .. T/N; ({:1:sycl::linearize2(gid1, lid1, T/N, N):}%4 == 0 ==>                      (lid1%32+32 <= 32 ==>sycl::linearize2(gid1, lid1, T/N, N)+32 <= |fxGs| ==>fx[sycl::linearize2(gid1, lid1, T/N, N)] == sum(fxGs[sycl::linearize2(gid1, lid1, T/N, N)   .. sycl::linearize2(gid1, lid1, T/N, N)+32]))));
    context_everywhere (\forall int lid1=0 .. N, int gid1=0 .. T/N; ({:2:sycl::linearize2(gid1, lid1, T/N, N):}%4 == 1 ==> (0 <= lid1%32 - 1 ==> lid1%32+32-1 <= 32 ==> sycl::linearize2(gid1, lid1, T/N, N)+32-1 <= |fyGs| ==>                             fx[sycl::linearize2(gid1, lid1, T/N, N)] == sum(fyGs[sycl::linearize2(gid1, lid1, T/N, N)-1 .. sycl::linearize2(gid1, lid1, T/N, N)+32-1]))));
    context_everywhere (\forall int lid1=0 .. N, int gid1=0 .. T/N; ({:3:sycl::linearize2(gid1, lid1, T/N, N):}%4 == 2 ==> (0 <= lid1%32 - 2 ==> lid1%32+32-2 <= 32 ==> sycl::linearize2(gid1, lid1, T/N, N)+32-2 <= |fzGs| ==> fx[sycl::linearize2(gid1, lid1, T/N, N)] == sum(fzGs[sycl::linearize2(gid1, lid1, T/N, N)-2 .. sycl::linearize2(gid1, lid1, T/N, N)+32-2]))));
    context_everywhere |fxGs| == Tf() && |fyGs| == Tf() && |fzGs| == Tf();

    ensures resultxx == sum(fxGs[0 .. Tf()]);
    ensures resultyy == sum(fyGs[0 .. Tf()]);
    ensures resultzz == sum(fzGs[0 .. Tf()]);
@*/
void accumulateResult(int T, int N, int* fx) {

    int resultx = 0;
    int resulty = 0;
    int resultz = 0;
    int gid = 0;
 
    /*@ loop_invariant 0 <= gid && gid <= T/N;
        loop_invariant (gid < T/N ) ==> (0 <= sycl::linearize2(gid, 0, T/N, N) && sycl::linearize2(gid, 0, T/N, N) < Tf());

        loop_invariant (gid < T/N) ==> sycl::linearize2(gid, 0, T/N, N)%4 == 0;
        loop_invariant (gid < T/N ) ==>  (resultx == sum(fxGs[0 .. sycl::linearize2(gid, 0, T/N, N)]) &&
                                          resultz == sum(fzGs[0 .. sycl::linearize2(gid, 0, T/N, N)]) &&
                                          resulty == sum(fyGs[0 .. sycl::linearize2(gid, 0, T/N, N)]));
        loop_invariant (gid == T/N ) ==> resultx == sum(fxGs[0 .. Tf()]) && resulty == sum(fyGs[0 .. Tf()]) && resultz == sum(fzGs[0 .. Tf()]); */
    for (gid=0; gid < T/N; gid++){
        int lid = 0;

        /*@ loop_invariant 0 <= gid && gid < T/N;
            loop_invariant 0 <= lid && lid <= N && lid%32==0;
            loop_invariant (lid < N ) ==> (0 <= sycl::linearize2(gid, lid, T/N, N) && sycl::linearize2(gid, lid, T/N, N) < Tf());
            
            loop_invariant (lid < N) ==> sycl::linearize2(gid, lid, T/N, N)%4 == 0;
            loop_invariant (lid < N ) ==> (resultx == sum(fxGs[0 .. sycl::linearize2(gid, lid, T/N, N)]) &&
                                           resulty == sum(fyGs[0 .. sycl::linearize2(gid, lid, T/N, N)]) &&
                                           resultz == sum(fzGs[0 .. sycl::linearize2(gid, lid, T/N, N)]));
            loop_invariant (lid == N ) ==> (resultx == sum(fxGs[0 .. sycl::linearize2(gid, N-32, T/N, N)+32]) &&
                                            resulty == sum(fyGs[0 .. sycl::linearize2(gid, N-32, T/N, N)+32]) &&
                                            resultz == sum(fzGs[0 .. sycl::linearize2(gid, N-32, T/N, N)+32])); */
        for (lid=0; lid < N; lid=lid+32){
            /*@ assert idshift(T,N,gid,lid); */
            //@ ghost int lidp1 = lid+1;
            //@ ghost int lidp2 = lid+2;


            resultx = resultx + fx[sycl::linearize2(gid, lid, T/N, N)];
            /*@ assert lemmaSumOverConcat(fxGs[0 .. sycl::linearize2(gid, lid, T/N, N)],fxGs[sycl::linearize2(gid, lid, T/N, N) .. sycl::linearize2(gid, lid, T/N, N) + 32]);
                assert lemmaSumOverABBCisAC(fxGs,0,sycl::linearize2(gid, lid, T/N, N),sycl::linearize2(gid, lid, T/N, N),sycl::linearize2(gid, lid, T/N, N)+32); */

            resulty = resulty + fx[sycl::linearize2(gid, lidp1, T/N, N)];
            /*@ assert lemmaSumOverConcat(fyGs[0 .. sycl::linearize2(gid, lid, T/N, N)],fyGs[sycl::linearize2(gid, lidp1, T/N, N)-1 .. sycl::linearize2(gid, lidp1, T/N, N)+32-1]);
                assert lemmaSumOverABBCisAC(fyGs,0,sycl::linearize2(gid, lid, T/N, N),sycl::linearize2(gid, lidp1, T/N, N)-1,sycl::linearize2(gid, lidp1, T/N, N)+32-1); */

            resultz = resultz + fx[sycl::linearize2(gid, lidp2, T/N, N)];
            /*@ assert lemmaSumOverConcat(fzGs[0 .. sycl::linearize2(gid, lid, T/N, N)],fzGs[sycl::linearize2(gid, lidp2, T/N, N)-2 .. sycl::linearize2(gid, lidp2, T/N, N)+32-2]);
                assert lemmaSumOverABBCisAC(fzGs,0,sycl::linearize2(gid, lid, T/N, N),sycl::linearize2(gid, lidp2, T/N, N)-2,sycl::linearize2(gid, lidp2, T/N, N)+32-2); */

            //@     ghost int lid2=lid+32;
            //@     assert (lid2 < N) ==> sycl::linearize2(gid, lid, T/N, N) + 32 == sycl::linearize2(gid, lid2, T/N, N);
            //@     assert (lid2 == N) ==> sycl::linearize2(gid, lid, T/N, N) + 32 == sycl::linearize2(gid, lid2-32, T/N, N)+32;
        }
        //@ assert gidshift(T,N,gid);
    }
    //@ ghost resultxx = resultx;
    //@ ghost resultyy = resulty;
    //@ ghost resultzz = resultz;
    
    //@ assert resultx == sum(fxGs[0 .. Tf()]);
    //@ assert resulty == sum(fyGs[0 .. Tf()]);
    //@ assert resultz == sum(fzGs[0 .. Tf()]);

}

/////////////////////////////////////////
/// Sum related functions and lemma's ///
/////////////////////////////////////////
/*@
ensures |xs| == 0 ==> \result == 0;
ensures |xs| == 1 ==> \result == xs[0];
opaque pure int sum(seq<int> xs) =
    0 < |xs| ? xs[0] + sum(xs[1 .. ]) : 0;

pure int incr1(int a) = a+1;
pure int incr2(int a) = a+2;


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

requires N > 0 && T > N && T%N == 0 && N%32==0 ;
requires 0 <= gid && gid < T/N;
requires 0 <= lid && lid < N && lid%32 == 0; 
ensures \result;
ensures sycl::linearize2(gid, lid, T/N, N)+32 <= T;
pure bool idshift(int T, int N,int gid, int lid) = true; 

requires N > 0 && T > N && T%N == 0 && N%32==0 ;
requires 0 <= gid && gid < T/N;
requires sycl::linearize2(gid, 0, T/N, N)%4 == 0;
ensures gid+1<T/N  ==> sycl::linearize2(gid+1, 0, T/N, N)%4 == 0;
pure bool gidshift(int T, int N,int gid) = true; 

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