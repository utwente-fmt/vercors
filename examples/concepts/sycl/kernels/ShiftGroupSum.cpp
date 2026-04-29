

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
function truncmod(a: Int, b: Int): Int
  requires b != 0
  decreases
{
  (let i ==
    (a % b) in
    (a >= 0 || i == 0 ? i : i - (b > 0 ? b : -b)))
}
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
                //@ inhale false;
            });});
        //@ assert true;
        e0.wait();
        //@ assert true;
    }

    /*@
        assert (\forall int lid1=0 .. N, int gid1=0 .. T/N;
            ({:1:sycl::linearize2(gid1, lid1, T/N, N):}%4 == 0 ==>(lid1%32+32 <= 32 ==>sycl::linearize2(gid1, lid1, T/N, N)+32 <= |fxGs()| ==>fx[sycl::linearize2(gid1, lid1, T/N, N)] == sum(fxGs()[sycl::linearize2(gid1, lid1, T/N, N)   .. sycl::linearize2(gid1, lid1, T/N, N)+32]))) &&
            ({:2:sycl::linearize2(gid1, lid1, T/N, N):}%4 == 1 ==> (0 <= lid1%32 - 1 ==> lid1%32+32-1 <= 32 ==> sycl::linearize2(gid1, lid1, T/N, N)+32-1 <= |fyGs()| ==>                             fx[sycl::linearize2(gid1, lid1, T/N, N)] == sum(fyGs()[sycl::linearize2(gid1, lid1, T/N, N)-1 .. sycl::linearize2(gid1, lid1, T/N, N)+32-1]))) &&
            ({:3:sycl::linearize2(gid1, lid1, T/N, N):}%4 == 2 ==> (0 <= lid1%32 - 2 ==> lid1%32+32-2 <= 32 ==> sycl::linearize2(gid1, lid1, T/N, N)+32-2 <= |fzGs()| ==> fx[sycl::linearize2(gid1, lid1, T/N, N)] == sum(fzGs()[sycl::linearize2(gid1, lid1, T/N, N)-2 .. sycl::linearize2(gid1, lid1, T/N, N)+32-2]))));
    */

    int resultx = 0;
    int gid = 0;

    /*@
        loop_invariant T == Tf() && N == Nf() && T > N && T%N == 0 && N%32==0 && N > 0;
        loop_invariant 0 <= gid && gid <= T/N;
        loop_invariant T%N==0 && N%32==0 && |fxGs()| == Tf();
        loop_invariant (gid < T/N ) ==> (0 <= sycl::linearize2(gid, 0, T/N, N) && sycl::linearize2(gid, 0, T/N, N) < Tf());
        loop_invariant \pointer(fx, T, 1\2);
        loop_invariant (\forall int lid1=0 .. N, int gid1=0 .. T/N;
            ({:1:sycl::linearize2(gid1, lid1, T/N, N):}%4 == 0 ==>                      (lid1%32+32 <= 32 ==>sycl::linearize2(gid1, lid1, T/N, N)+32 <= |fxGs()| ==>fx[sycl::linearize2(gid1, lid1, T/N, N)] == sum(fxGs()[sycl::linearize2(gid1, lid1, T/N, N)   .. sycl::linearize2(gid1, lid1, T/N, N)+32]))) &&
            ({:2:sycl::linearize2(gid1, lid1, T/N, N):}%4 == 1 ==> (0 <= lid1%32 - 1 ==> lid1%32+32-1 <= 32 ==> sycl::linearize2(gid1, lid1, T/N, N)+32-1 <= |fyGs()| ==>                             fx[sycl::linearize2(gid1, lid1, T/N, N)] == sum(fyGs()[sycl::linearize2(gid1, lid1, T/N, N)-1 .. sycl::linearize2(gid1, lid1, T/N, N)+32-1]))) &&
            ({:3:sycl::linearize2(gid1, lid1, T/N, N):}%4 == 2 ==> (0 <= lid1%32 - 2 ==> lid1%32+32-2 <= 32 ==> sycl::linearize2(gid1, lid1, T/N, N)+32-2 <= |fzGs()| ==> fx[sycl::linearize2(gid1, lid1, T/N, N)] == sum(fzGs()[sycl::linearize2(gid1, lid1, T/N, N)-2 .. sycl::linearize2(gid1, lid1, T/N, N)+32-2]))));

        loop_invariant (gid < T/N) ==> sycl::linearize2(gid, 0, T/N, N)%4 == 0;
        loop_invariant (gid < T/N ) ==> resultx == sum(fxGs()[0 .. sycl::linearize2(gid, 0, T/N, N)]);
        loop_invariant (gid == T/N ) ==> resultx == sum(fxGs()[0 .. Tf()]);
    */
    for (gid=0; gid < T/N; gid++){
        int lid = 0;

        /*@ loop_invariant T == Tf() && N == Nf() && T > N && T%N == 0 && N%32==0 && N > 0;
            loop_invariant 0 <= gid && gid < T/N;
            loop_invariant 0 <= lid && lid <= N && lid%32==0;
            loop_invariant T%N==0 && N%32==0 && |fxGs()| == Tf();
            loop_invariant (lid < N ) ==> (0 <= sycl::linearize2(gid, lid, T/N, N) && sycl::linearize2(gid, lid, T/N, N) < Tf());

            loop_invariant \pointer(fx, T, 1\4);
            loop_invariant (\forall int lid1=0 .. N, int gid1=0 .. T/N;
                ({:1:sycl::linearize2(gid1, lid1, T/N, N):}%4 == 0 ==>(lid1%32+32 <= 32 ==>sycl::linearize2(gid1, lid1, T/N, N)+32 <= |fxGs()| ==>fx[sycl::linearize2(gid1, lid1, T/N, N)] == sum(fxGs()[sycl::linearize2(gid1, lid1, T/N, N)   .. sycl::linearize2(gid1, lid1, T/N, N)+32]))) &&
                ({:2:sycl::linearize2(gid1, lid1, T/N, N):}%4 == 1 ==> (0 <= lid1%32 - 1 ==> lid1%32+32-1 <= 32 ==> sycl::linearize2(gid1, lid1, T/N, N)+32-1 <= |fyGs()| ==>                             fx[sycl::linearize2(gid1, lid1, T/N, N)] == sum(fyGs()[sycl::linearize2(gid1, lid1, T/N, N)-1 .. sycl::linearize2(gid1, lid1, T/N, N)+32-1]))) &&
                ({:3:sycl::linearize2(gid1, lid1, T/N, N):}%4 == 2 ==> (0 <= lid1%32 - 2 ==> lid1%32+32-2 <= 32 ==> sycl::linearize2(gid1, lid1, T/N, N)+32-2 <= |fzGs()| ==> fx[sycl::linearize2(gid1, lid1, T/N, N)] == sum(fzGs()[sycl::linearize2(gid1, lid1, T/N, N)-2 .. sycl::linearize2(gid1, lid1, T/N, N)+32-2]))));

            
            loop_invariant (lid < N) ==> sycl::linearize2(gid, lid, T/N, N)%4 == 0;
            loop_invariant (lid < N ) ==>  resultx == sum(fxGs()[0 .. sycl::linearize2(gid, lid, T/N, N)]);
            loop_invariant (lid == N ) ==> resultx == sum(fxGs()[0 .. sycl::linearize2(gid, N-32, T/N, N)+32]);               
        */
        for (lid=0; lid < N; lid=lid+32){
            /*@
            assert idshift(T,N,gid,lid);
            ghost int resultBU = resultx;
            */
            resultx = resultx + fx[sycl::linearize2(gid, lid, T/N, N)];
            /*@
                assert resultx == resultBU + fx[{:sycl::linearize2(gid, lid, T/N, N):}];
                assert lemmaSumOverConcat(
                    fxGs()[0 .. sycl::linearize2(gid, lid, T/N, N)],
                    fxGs()[sycl::linearize2(gid, lid, T/N, N) .. sycl::linearize2(gid, lid, T/N, N) + 32]
                );

                assert lemmaSumOverABBCisAC(
                    fxGs(),
                    0,
                    sycl::linearize2(gid, lid, T/N, N),
                    sycl::linearize2(gid, lid, T/N, N),
                    sycl::linearize2(gid, lid, T/N, N)+32
                );
            */

            //@     ghost int lid2=lid+32;
            //@     assert (lid2 < N) ==> sycl::linearize2(gid, lid, T/N, N) + 32 == sycl::linearize2(gid, lid2, T/N, N);
            //@     assert (lid2 < N ) ==>  resultx == sum(fxGs()[0 .. sycl::linearize2(gid, lid2, T/N, N)]);
            //@     assert (lid2 == N) ==> sycl::linearize2(gid, lid, T/N, N) + 32 == sycl::linearize2(gid, lid2-32, T/N, N)+32;
            //@     assert (lid2 == N) ==> resultx == sum(fxGs()[0 .. sycl::linearize2(gid, lid2-32, T/N, N)+32]);

        }
        //@ assert gidshift(T,N,gid);
    }
    // assert resultx == sum(fxGs()[0 .. Tf()]);

    //@ inhale false;
    // assert resulty == sum(fyGs()[0 .. Tf()]);
    // assert resultz == sum(fzGs()[0 .. Tf()]);
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