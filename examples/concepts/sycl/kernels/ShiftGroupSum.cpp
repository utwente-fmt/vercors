#include <sycl/sycl.hpp>

/*@
ensures |\result|>0;
pure seq<int> fxGs();
ensures |\result|>0;
pure seq<int> fyGs();
ensures |\result|>0;
pure seq<int> fzGs();
*/
/*@ yields int resultxx;
    yields int resultyy;
    yields int resultzz;
    context_everywhere T > 0 && N > 0 && T > N && T/N > 0 && T%N == 0 && N%sycl::h::warp_sizes()==0;
    context_everywhere fx != fy && fx != fz && fy != fz;
    context \pointer(fx, T, write) ** \pointer(fy, T, write) ** \pointer(fz, T, write);
    requires |fxGs()| == T && (\forall int i=0 .. T; fxGs()[i] == {:fx[i]:});
    requires |fyGs()| == T && (\forall int i=0 .. T; fyGs()[i] == {:fy[i]:});
    requires |fzGs()| == T && (\forall int i=0 .. T; fzGs()[i] == fz[i]);

    ensures |fxGs()| == T && (\forall int i=0 .. T; fxGs()[i] == \old(fx[i]));
    ensures |fyGs()| == T && (\forall int i=0 .. T; fyGs()[i] == \old(fy[i]));
    ensures |fzGs()| == T && (\forall int i=0 .. T; fzGs()[i] == \old(fz[i]));

    ensures resultxx == sum(fxGs()[0 .. T]);
    ensures resultyy == sum(fyGs()[0 .. T]);
    ensures resultzz == sum(fzGs()[0 .. T]);
@*/
void smartsum(sycl::queue q, int T, int N, int* fx, int* fy, int* fz) {
    //@ label bK;
    //@ assert true;
    {
        sycl::buffer<int, 1> fxBuf = sycl::buffer(fx, sycl::range<1>(T));
        sycl::buffer<int, 1> fyBuf = sycl::buffer(fy, sycl::range<1>(T));
        sycl::buffer<int, 1> fzBuf = sycl::buffer(fz, sycl::range<1>(T));

        //@ assume fxBuf != fyBuf && fyBuf != fzBuf && fxBuf != fzBuf;
        sycl::event e0 = q.submit([&](sycl::handler& h) {
            sycl::accessor<int, 1, sycl::access_mode::read_write> fxAcc = sycl::accessor(fxBuf, h, sycl::read_write);
            sycl::accessor<int, 1, sycl::access_mode::read_write> fyAcc = sycl::accessor(fyBuf, h, sycl::read_write);
            sycl::accessor<int, 1, sycl::access_mode::read_write> fzAcc = sycl::accessor(fzBuf, h, sycl::read_write);
            
            h.parallel_for(sycl::nd_range<1>(sycl::range<1>(T), sycl::range<1>(N)),
            /*@ context Perm(fxAcc[it.get_global_id(0)], write) ** Perm(fyAcc[it.get_global_id(0)], write) ** Perm(fzAcc[it.get_global_id(0)], write);
                ensures fxAcc == \old(fxAcc) ** fyAcc == \old(fyAcc) ** fzAcc == \old(fzAcc);
                context fxAcc != fyAcc && fyAcc != fzAcc && fxAcc != fzAcc;
                context sycl::h::wrpsz_pow() >= 3;
                requires |fxGs()| == T && fxGs()[it.get_global_id(0)] == fxAcc[it.get_global_id(0)] && fxGs()[it.get_global_id(0)] == \old(fx[it.get_global_id(0)]);
                requires |fyGs()| == T && fyGs()[it.get_global_id(0)] == fyAcc[it.get_global_id(0)] && fyGs()[it.get_global_id(0)] == \old(fy[it.get_global_id(0)]);
                requires |fzGs()| == T && fzGs()[it.get_global_id(0)] == fzAcc[it.get_global_id(0)] && fzGs()[it.get_global_id(0)] == \old(fz[it.get_global_id(0)]);
                context 0 <= it.get_global_id(0) && it.get_global_id(0) < T;
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
                        fxAcc[it.get_global_id(0)] == sum(fzGs()[it.get_global_id(0)-2 .. it.get_global_id(0)+it.get_sub_group().get_local_range(0)-2])); */
            [=](sycl::nd_item<1> it) {
                //@ assert true;
                sycl::sub_group sg = it.get_sub_group();
                int gid = it.get_global_id(0);
                int laneId = sg.get_local_id();
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
                    (\sgtid + d1 < sg.get_local_range(0) ==> \gtid+d2 <= |fzGs()| ==> \sg_val == sum(fxGs()[\gtid .. \gtid+d2])) :
                    (0 <= \sgtid - d1 ==> \sg_val == sum(fyGs()[\gtid-d1 .. \gtid+1])) } */;

                fzAcc[gid] += sycl::shift_group_right(sg, fzAcc[gid], d2)
                /*@ sub_group_inv { \sgtid+d1 < sg.get_local_range(0) ==> \gtid+d2 <= |fzGs()| ==> \sg_val == sum(fzGs()[\gtid .. \gtid+d2]) } */;

                /*@ assert lemmaSumOverConcat(fxGs()[gid    .. gid+d2],   fxGs()[gid+d2 .. gid+d2+d2])  && lemmaSumOverABBCisAC(fxGs(), gid,    gid+d2, gid+d2, gid+d2+d2);
                    assert lemmaSumOverConcat(fyGs()[gid-d1 .. gid+1],    fyGs()[gid+1  .. gid+d2+1])   && lemmaSumOverABBCisAC(fyGs(), gid-d1, gid+1,  gid+d2-d1, gid+d2+1);
                    assert lemmaSumOverConcat(fzGs()[gid-d2 .. gid-d2+d2],fzGs()[gid    .. gid+d2])     && lemmaSumOverABBCisAC(fzGs(), gid-d2, gid,    gid, gid+d2);
                    assert true; */

                if (gid % 4 >= 2) { fxAcc[gid] = fzAcc[gid]; }

                int dk = 4;
                //@ ghost int k1 = 2;

                /*@ frame 
                    context T > 0 && N > 0 && T > N && T/N > 0 && T%N == 0 && N%sycl::h::warp_sizes()==0;
                    context gid == it.get_global_id(0);
                    context laneId == sg.get_local_id();
                    context 0 <= it.get_global_id(0) && it.get_global_id(0) < T;
                    context sycl::h::wrpsz_pow() >= 3;
                    context 4 <= dk && dk <= sg.get_local_range(0);
                    context dk%4==0;
                    requires k1 == 2 && dk == 4;
                    context k1 >= 2 && dk == sycl::h::exp(2,k1) && k1 <= sycl::h::wrpsz_pow();
                    context sg.get_local_range(0) %2==0;
                    context dk == sg.get_local_range(0) ==> sycl::h::exp(2,k1) == sg.get_local_range(0);
                    context dk >= sg.get_local_range(0) ==> dk == sg.get_local_range(0);
                    context dk < sg.get_local_range(0) ==> k1 < sycl::h::wrpsz_pow();
                    context Perm(fxAcc[gid], write);
                    context |fzGs()| == |fyGs()| && |fxGs()| == |fyGs()| && |fzGs()| == T;
                    context (gid%4 == 0) ==> (laneId+dk   <= sg.get_local_range(0) && gid+dk   <= |fxGs()|) ==> fxAcc[gid] == sum(fxGs()[gid   .. gid+dk]);
                    context (gid%4 == 1) ==> (0 <= laneId - 1 && laneId+dk-1 <= sg.get_local_range(0) && gid+dk-1 <= |fyGs()|) ==> fxAcc[gid] == sum(fyGs()[gid-1 .. gid+dk-1]);
                    context (gid%4 == 2) ==> (0 <= laneId - 2 && laneId+dk-2 <= sg.get_local_range(0) && gid+dk-2 <= |fzGs()|) ==> fxAcc[gid] == sum(fzGs()[gid-2 .. gid+dk-2]);
                    ensures dk == sg.get_local_range(0);
                {*/
                /*@ loop_invariant T > 0 && N > 0 && T > N && T/N > 0 && T%N == 0 && N%sycl::h::warp_sizes()==0;
                    loop_invariant gid == it.get_global_id(0);
                    loop_invariant laneId == sg.get_local_id();
                    loop_invariant 0 <= it.get_global_id(0) && it.get_global_id(0) < T;
                    loop_invariant sycl::h::wrpsz_pow() >= 3;
                    loop_invariant 4 <= dk && dk <= sg.get_local_range(0);
                    loop_invariant dk%4==0;
                    loop_invariant k1 >= 2 && dk == sycl::h::exp(2,k1) && k1 <= sycl::h::wrpsz_pow();
                    loop_invariant sg.get_local_range(0) %2==0;
                    loop_invariant dk == sg.get_local_range(0) ==> sycl::h::exp(2,k1) == sg.get_local_range(0);
                    loop_invariant dk >= sg.get_local_range(0) ==> dk == sg.get_local_range(0);
                    loop_invariant dk < sg.get_local_range(0) ==> k1 < sycl::h::wrpsz_pow();
                    loop_invariant Perm(fxAcc[gid], write);
                    
                    loop_invariant |fzGs()| == |fyGs()| && |fxGs()| == |fyGs()| && |fzGs()| == T;
                    loop_invariant (gid%4 == 0) ==> (laneId+dk   <= sg.get_local_range(0) && gid+dk   <= |fxGs()|) ==> fxAcc[gid] == sum(fxGs()[gid   .. gid+dk]);
                    loop_invariant (gid%4 == 1) ==> (0 <= laneId - 1 && laneId+dk-1 <= sg.get_local_range(0) && gid+dk-1 <= |fyGs()|) ==> fxAcc[gid] == sum(fyGs()[gid-1 .. gid+dk-1]);
                    loop_invariant (gid%4 == 2) ==> (0 <= laneId - 2 && laneId+dk-2 <= sg.get_local_range(0) && gid+dk-2 <= |fzGs()|) ==> fxAcc[gid] == sum(fzGs()[gid-2 .. gid+dk-2]);
                */ 
                for (dk = 4; dk < sg.get_local_range(0); dk = 2 * dk) {
                    int sgl_result2 = sycl::shift_group_left(sg, fxAcc[gid], dk)
                        /*@ sub_group_inv {   (\gtid%4 == 0) ?                     (\sgtid+dk   <= sg.get_local_range(0) ==> \gtid + dk <= |fxGs()| ==> \sg_val == sum(fxGs()[\gtid   .. \gtid+dk]))
                                            : (\gtid%4 == 1) ? (0 <= \sgtid - 1 ==> \sgtid+dk-1 <= sg.get_local_range(0) ==> \gtid+dk-1 <= |fyGs()| ==> \sg_val == sum(fyGs()[\gtid-1 .. \gtid+dk-1]))
                                            : (\gtid%4 == 2) ? (0 <= \sgtid - 2 ==> \sgtid+dk-2 <= sg.get_local_range(0) ==> \gtid+dk-2 <= |fzGs()| ==> \sg_val == sum(fzGs()[\gtid-2 .. \gtid+dk-2]))
                                            : true } */;
                    //@ ghost int gsfxAcc = fxAcc[gid];                                         
                    fxAcc[gid] += sgl_result2;

                     /*@ assert lemmaSumOverConcat(fxGs()[gid   .. gid+dk  ],fxGs()[gid+dk   .. gid+dk+dk  ]) && lemmaSumOverABBCisAC(fxGs(), gid  , gid+dk  , gid+dk  , gid+dk+dk  );
                            assert lemmaSumOverConcat(fyGs()[gid-1 .. gid+dk-1],fyGs()[gid+dk-1 .. gid+dk+dk-1]) && lemmaSumOverABBCisAC(fyGs(), gid-1, gid+dk-1, gid+dk-1, gid+dk+dk-1);
                            assert lemmaSumOverConcat(fzGs()[gid-2 .. gid+dk-2],fzGs()[gid+dk-2 .. gid+dk+dk-2]) && lemmaSumOverABBCisAC(fzGs(), gid-2, gid+dk-2, gid+dk-2, gid+dk+dk-2);
                            assert true; */

                    /*@ assert laneId + dk < sg.get_local_range(0) ==>
                                ((gid + dk) % 4 == 0
                                    ? laneId + dk + dk <= sg.get_local_range(0) ==>
                                        gid + dk + dk <= |fxGs()| ==>
                                            sgl_result2 == sum(fxGs()[gid + dk..gid + dk + dk])
                                    : 
                                    (gid + dk) % 4 == 1
                                    ? 0 <= laneId + dk - 1 ==>
                                        laneId + dk + dk - 1 <= sg.get_local_range(0) ==>
                                        gid + dk + dk - 1 <= |fyGs()| ==>
                                        sgl_result2 == sum(fyGs()[gid + dk - 1 .. gid + dk + dk - 1])
                                    : 
                                    (gid + dk) % 4 == 2
                                    ? 0 <= laneId + dk - 2 ==>
                                        laneId + dk + dk - 2 <= sg.get_local_range(0) ==>
                                        gid + dk + dk - 2 <= |fzGs()| ==>
                                        sgl_result2 == sum(fzGs()[gid + dk - 2 .. gid + dk + dk - 2])
                                            : true);
                    
                                                            */

                    /*@
                        assert mod_times_two(dk);

                        ghost if (gid%4==0) {
                            assert mod_add_0(gid,dk);
                            assert sycl::h::add(gid,dk)%4==0;
                            assert mod_add_0(gid,dk) ==> (gid+dk)%4 == 0;
                            assert (gid%4 == 0) ==> (laneId+dk+dk   <= sg.get_local_range(0) && gid+dk+dk   <= |fxGs()|) ==> fxAcc[gid] == sum(fxGs()[gid   .. gid+dk+dk]);
                        } else if (gid%4==1) {
                            assert mod_add_1(gid,dk);
                            assert sycl::h::add(gid,dk)%4==1;
                            assert mod_add_1(gid,dk) ==> (gid+dk)%4==1;
                            assert (gid%4 == 1) ==> (0 <= laneId - 1 && laneId+dk+dk-1 <= sg.get_local_range(0) && gid+dk+dk-1 <= |fyGs()|) ==> fxAcc[gid] == sum(fyGs()[gid-1 .. gid+dk+dk-1]);
                        } else if (gid%4==2) {
                            assert mod_add_2(gid,dk);
                            assert sycl::h::add(gid,dk)%4==2;
                            assert mod_add_2(gid,dk) ==> (gid+dk)%4==2;
                            assert (gid%4 == 2) ==> (0 <= laneId - 2 && laneId+dk+dk-2 <= sg.get_local_range(0) && gid+dk+dk-2 <= |fzGs()|) ==> fxAcc[gid] == sum(fzGs()[gid-2 .. gid+dk+dk-2]);
                        }
                    */
                    
                    //@ ghost if (k1+1<sycl::h::wrpsz_pow()) {expMonotonicity(k1+1, sycl::h::wrpsz_pow());}
                    /*@ ghost k1=k1+1; */
                    //@ assert mod_times_two(dk);
                }/*@}*/
                //@ assert true;
            });});
        //@ assert true;
        e0.wait();
        //@ assert true;
    }
    accumulateResult(T,N,fx) 
        /*@ given {fxGs=fxGs(), fyGs=fyGs(), fzGs=fzGs()}*/
        /*@ yields { resultxx = resultxx, resultyy = resultyy, resultzz = resultzz  } */;
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
ensures xs[a .. d] == xs[a .. b] + xs[c .. d];
pure bool lemmaSumOverABBCisAC(seq<int> xs, int a, int b, int c, int d);


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
pure bool idshift(int T, int N,int gid, int lid, int warpsz);


requires k >= 0 && l > 0 && k%l==0;
ensures (k+l)%l==0;
ensures \result;
pure bool modwrp(int k, int l);


requires wrpsz > 0 && 0 <= lid && lid < N && lid%wrpsz == 0; 
requires N > 0 &&  N%wrpsz==0;
ensures lid+wrpsz <= N;
ensures \result;
pure bool lidshift(int lid, int N, int wrpsz);


requires wsz == sycl::h::warp_sizes() && T > 0 && N > 0 && T > N && T%N == 0 && N%wsz==0 && N > 0 && |fxGs| == T;
requires \pointer(fx, T, read);
 pure bool intermediateResults1(int T, int N, int wsz, int* fx, seq<int> fxGs, seq<int> fyGs, seq<int> fzGs) = 
(\forall int lid1=0 .. N, int gid1=0 .. T/N; ({:1:sycl::linearize2(gid1, lid1, T/N, N):}%4 == 0 ==> 
    (lid1%wsz+wsz <= wsz ==>
    sycl::linearize2(gid1, lid1, T/N, N)+wsz <= |fxGs| ==>
        fx[sycl::linearize2(gid1, lid1, T/N, N)] == sum(fxGs[sycl::linearize2(gid1, lid1, T/N, N)   .. sycl::linearize2(gid1, lid1, T/N, N)+wsz]))));


requires wsz == sycl::h::warp_sizes() && T > 0 && N > 0 && T > N && T%N == 0 && N%wsz==0 && N > 0 && |fxGs| == T;
requires \pointer(fx, T, read);
 pure bool intermediateResults2(int T, int N, int wsz, int* fx, seq<int> fxGs, seq<int> fyGs, seq<int> fzGs) = 
(\forall int lid1=0 .. N, int gid1=0 .. T/N; ({:2:sycl::linearize2(gid1, lid1, T/N, N):}%4 == 1 ==> 
    (0 <= lid1%wsz - 1 ==> 
    lid1%wsz+wsz-1 <= wsz ==> 
    sycl::linearize2(gid1, lid1, T/N, N)+wsz-1 <= |fyGs| ==>                             
        fx[sycl::linearize2(gid1, lid1, T/N, N)] == sum(fyGs[sycl::linearize2(gid1, lid1, T/N, N)-1 .. sycl::linearize2(gid1, lid1, T/N, N)+wsz-1]))));


requires wsz == sycl::h::warp_sizes() && T > 0 && N > 0 && T > N && T%N == 0 && N%wsz==0 && N > 0 && |fxGs| == T;
requires \pointer(fx, T, read);
 pure bool intermediateResults3(int T, int N, int wsz, int* fx, seq<int> fxGs, seq<int> fyGs, seq<int> fzGs) = 
(\forall int lid1=0 .. N, int gid1=0 .. T/N; ({:3:sycl::linearize2(gid1, lid1, T/N, N):}%4 == 2 ==> 
    (0 <= lid1%wsz - 2 ==> 
    lid1%wsz+wsz-2 <= wsz ==> 
    sycl::linearize2(gid1, lid1, T/N, N)+wsz-2 <= |fzGs| ==> 
        fx[sycl::linearize2(gid1, lid1, T/N, N)] == sum(fzGs[sycl::linearize2(gid1, lid1, T/N, N)-2 .. sycl::linearize2(gid1, lid1, T/N, N)+wsz-2]))));



requires wsz==sycl::h::warp_sizes();
requires wsz >= 8;
requires N >0 && N%wsz==0;
requires 0 <= lid && lid <= N && lid%wsz==0;
ensures \result;
ensures 0 == (lid+1)%wsz - 1;
ensures 0 == (lid+2)%wsz - 2;
ensures (lid+1)%wsz+wsz-1 <= wsz; 
ensures (lid+2)%wsz+wsz-2 <= wsz; 
pure bool idshiftBack(int lid, int wsz, int N);

requires T > 0 && N >0 && T > N && T%N == 0 && N%sycl::h::warp_sizes()==0;
requires 0 <= gid && gid < T/N;
requires sycl::linearize2(gid, 0, T/N, N)%4 == 0;
ensures \result;
ensures mod_trans(N,sycl::h::warp_sizes(),4);
ensures gid+1<T/N ==> sycl::linearize2(gid+1, 0, T/N, N)%4 == 0;
pure bool gidshiftPlease(int gid, int T, int N);

requires c>0 && b > c && a >=0;
requires a%b==0 && b%c==0;
ensures \result;
ensures a%c==0;
pure bool mod_trans(int a, int b, int c);

requires g >=0 && d >= 0 && 0 <= x && x < 4 && 0 <= y && y < 4;
requires g%4==x && d%4==y;
ensures (g+d)%4==(x+y)%4;
ensures sycl::h::add(g,d)%4==sycl::h::add(x,y)%4;
ensures \result;
pure bool mod_add(int g, int d, int x, int y);

requires g >=0 && d >= 0;
requires g%4==0 && d%4==0;
ensures (g+d)%4==0;
ensures \result;
ensures sycl::h::add(g,d)%4==0;
pure bool mod_add_0(int g, int d) = mod_add(g,d,0,0);

requires g >=0 && d >= 0;
requires g%4==1 && d%4==0;
ensures (g+d)%4==1;
ensures \result;
ensures sycl::h::add(g,d)%4==1;
pure bool mod_add_1(int g, int d) = mod_add(g,d,1,0);

requires g >=0 && d >= 0;
requires g%4==2 && d%4==0;
ensures (g+d)%4==2;
ensures \result;
ensures sycl::h::add(g,d)%4==2;
pure bool mod_add_2(int g, int d) = mod_add(g,d,2,0);


requires g > 0 && g % 4 == 0;
ensures \result;
ensures (2*g)%4 == 0;
pure bool mod_times_two(int g);
*/


/*
Proofs using ChatGPT, I'm not that good at Lean
idshiftBack on its own verifies, but takes 1:27 by itself and non-deterministically gets stuck , so I leave it abstract.
mod_trans is proven in Lean4, https://github.com/leanprover/lean4/blob/3dc1a088b6d2d8eafe25a7cd7ec7b58d731bd7cc/src/Init/Data/Int/DivMod/Bootstrap.lean#L37-L38


import Mathlib.Data.Nat.Basic

theorem mod4_add_general (g d x y : ℕ)
  (hg : g % 4 = x)
  (hd : d % 4 = y) :
  (g + d) % 4 = (x + y) % 4 := by
  have h := Nat.add_mod g d 4
  -- rewrite both sides using assumptions
  rw [hg, hd] at h
  exact h
  */



/*@
    given seq<int> fxGs;
    given seq<int> fyGs;
    given seq<int> fzGs;
    yields int resultxx;
    yields int resultyy;
    yields int resultzz;
    context_everywhere T > 0 && N >0 && T > N && T%N == 0 && N%sycl::h::warp_sizes()==0;
    context \pointer(fx, T, 1\2);
    requires intermediateResults1(T,N,sycl::h::warp_sizes(),fx,fxGs,fyGs,fzGs);
    requires intermediateResults2(T,N,sycl::h::warp_sizes(),fx,fxGs,fyGs,fzGs);
    requires intermediateResults3(T,N,sycl::h::warp_sizes(),fx,fxGs,fyGs,fzGs);
    
    context (\forall int lid1=0 .. N, int gid1=0 .. T/N; ({:1:sycl::linearize2(gid1, lid1, T/N, N):}%4 == 0 ==>                      (lid1%sycl::h::warp_sizes()+sycl::h::warp_sizes() <= sycl::h::warp_sizes() ==>sycl::linearize2(gid1, lid1, T/N, N)+sycl::h::warp_sizes() <= |fxGs| ==>fx[sycl::linearize2(gid1, lid1, T/N, N)] == sum(fxGs[sycl::linearize2(gid1, lid1, T/N, N)   .. sycl::linearize2(gid1, lid1, T/N, N)+sycl::h::warp_sizes()]))));
    context (\forall int lid1=0 .. N, int gid1=0 .. T/N; ({:2:sycl::linearize2(gid1, lid1, T/N, N):}%4 == 1 ==> (0 <= lid1%sycl::h::warp_sizes() - 1 ==> lid1%sycl::h::warp_sizes()+sycl::h::warp_sizes()-1 <= sycl::h::warp_sizes() ==> sycl::linearize2(gid1, lid1, T/N, N)+sycl::h::warp_sizes()-1 <= |fyGs| ==>                             fx[sycl::linearize2(gid1, lid1, T/N, N)] == sum(fyGs[sycl::linearize2(gid1, lid1, T/N, N)-1 .. sycl::linearize2(gid1, lid1, T/N, N)+sycl::h::warp_sizes()-1]))));
    context (\forall int lid1=0 .. N, int gid1=0 .. T/N; ({:3:sycl::linearize2(gid1, lid1, T/N, N):}%4 == 2 ==> (0 <= lid1%sycl::h::warp_sizes() - 2 ==> lid1%sycl::h::warp_sizes()+sycl::h::warp_sizes()-2 <= sycl::h::warp_sizes() ==> sycl::linearize2(gid1, lid1, T/N, N)+sycl::h::warp_sizes()-2 <= |fzGs| ==> fx[sycl::linearize2(gid1, lid1, T/N, N)] == sum(fzGs[sycl::linearize2(gid1, lid1, T/N, N)-2 .. sycl::linearize2(gid1, lid1, T/N, N)+sycl::h::warp_sizes()-2]))));
    context_everywhere |fxGs| == T && |fyGs| == T && |fzGs| == T;

    ensures resultxx == sum(fxGs[0 .. T]);
    ensures resultyy == sum(fyGs[0 .. T]);
    ensures resultzz == sum(fzGs[0 .. T]);
@*/
void accumulateResult(int T, int N, int* fx);