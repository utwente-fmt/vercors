

#include <sycl/sycl.hpp>

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
ensures \result;
pure bool mod_add(int g, int d, int x, int y);


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
void accumulateResult(int T, int N, int* fx) {
    int resultx = 0;
    int resulty = 0;
    int resultz = 0;
    int gid = 0;
 
    /*@ loop_invariant 0 <= gid && gid <= T/N;
        loop_invariant (gid < T/N ) ==> (0 <= sycl::linearize2(gid, 0, T/N, N) && sycl::linearize2(gid, 0, T/N, N) < T);
        loop_invariant \pointer(fx, T, 1\2);
        loop_invariant intermediateResults1(T,N,sycl::h::warp_sizes(),fx,fxGs,fyGs,fzGs);        
        loop_invariant intermediateResults2(T,N,sycl::h::warp_sizes(),fx,fxGs,fyGs,fzGs);        
        loop_invariant intermediateResults3(T,N,sycl::h::warp_sizes(),fx,fxGs,fyGs,fzGs);        

        loop_invariant (gid < T/N) ==> sycl::linearize2(gid, 0, T/N, N)%4 == 0;
        loop_invariant (gid < T/N ) ==>  resultx == sum(fxGs[0 .. sycl::linearize2(gid, 0, T/N, N)]);
        loop_invariant (gid < T/N ) ==>  resultz == sum(fzGs[0 .. sycl::linearize2(gid, 0, T/N, N)]);
        loop_invariant (gid < T/N ) ==>  resulty == sum(fyGs[0 .. sycl::linearize2(gid, 0, T/N, N)]);
                                          
        loop_invariant (gid == T/N ) ==> resultx == sum(fxGs[0 .. T]);
        loop_invariant (gid == T/N ) ==> resulty == sum(fyGs[0 .. T]);
        loop_invariant (gid == T/N ) ==> resultz == sum(fzGs[0 .. T]); 
         */
    for (gid=0; gid < T/N; gid++){
        int lid = 0;

        /*@ assert (lid < N ) ==> resultx == sum(fxGs[0 .. sycl::linearize2(gid, lid, T/N, N)]);
            assert (lid < N ) ==> resulty == sum(fyGs[0 .. sycl::linearize2(gid, lid, T/N, N)]);
            assert (lid < N ) ==> resultz == sum(fzGs[0 .. sycl::linearize2(gid, lid, T/N, N)]);
        */

        /*@ loop_invariant 0 <= gid && gid < T/N;
            loop_invariant 0 <= lid && lid <= N && lid%sycl::h::warp_sizes()==0;
            loop_invariant (lid < N ) ==> (0 <= sycl::linearize2(gid, lid, T/N, N) && sycl::linearize2(gid, lid, T/N, N) < T);
            loop_invariant \pointer(fx, T, 1\2);
            loop_invariant intermediateResults1(T,N,sycl::h::warp_sizes(),fx,fxGs,fyGs,fzGs);
            loop_invariant intermediateResults2(T,N,sycl::h::warp_sizes(),fx,fxGs,fyGs,fzGs);
            loop_invariant intermediateResults3(T,N,sycl::h::warp_sizes(),fx,fxGs,fyGs,fzGs);
            
            loop_invariant (lid < N) ==> sycl::linearize2(gid, lid, T/N, N)%4 == 0;
            loop_invariant (lid < N) ==> (sycl::linearize2(gid, lid, T/N, N)+1)%4 == 1;
            loop_invariant (lid < N) ==> (sycl::linearize2(gid, lid, T/N, N)+2)%4 == 2;
        
            loop_invariant (lid < N ) ==> resultx == sum(fxGs[0 .. sycl::linearize2(gid, lid, T/N, N)]);
            loop_invariant (lid < N ) ==> resulty == sum(fyGs[0 .. sycl::linearize2(gid, lid, T/N, N)]);
            loop_invariant (lid < N ) ==> resultz == sum(fzGs[0 .. sycl::linearize2(gid, lid, T/N, N)]);
                                           
            loop_invariant (lid == N ) ==> resultx == sum(fxGs[0 .. sycl::linearize2(gid, N-sycl::h::warp_sizes(), T/N, N)+sycl::h::warp_sizes()]);
            loop_invariant (lid == N ) ==> resulty == sum(fyGs[0 .. sycl::linearize2(gid, N-sycl::h::warp_sizes(), T/N, N)+sycl::h::warp_sizes()]);
            loop_invariant (lid == N ) ==> resultz == sum(fzGs[0 .. sycl::linearize2(gid, N-sycl::h::warp_sizes(), T/N, N)+sycl::h::warp_sizes()]);  */
        for (lid=0; lid < N; lid=lid+sycl::h::warp_sizes()){
            /*@ assert idshift(T,N,gid,lid,sycl::h::warp_sizes()); */
            //@ assert lidshift(lid,N,sycl::h::warp_sizes());
            /*@ assert modwrp(lid, sycl::h::warp_sizes()); */
            //@ ghost int lidp1 = lid+1;
            //@ ghost int lidp2 = lid+2;


            resultx = resultx + fx[sycl::linearize2(gid, lid, T/N, N)];
            /*@ assert lemmaSumOverConcat(fxGs[0 .. sycl::linearize2(gid, lid, T/N, N)],fxGs[sycl::linearize2(gid, lid, T/N, N) .. sycl::linearize2(gid, lid, T/N, N) + sycl::h::warp_sizes()]);
                assert lemmaSumOverABBCisAC(fxGs,0,sycl::linearize2(gid, lid, T/N, N),sycl::linearize2(gid, lid, T/N, N),sycl::linearize2(gid, lid, T/N, N)+sycl::h::warp_sizes()); */
            //@ assert intermediateResults1(T,N,sycl::h::warp_sizes(),fx,fxGs,fyGs,fzGs); 
            //@ assert (lid+sycl::h::warp_sizes() < N ) ==> resultx == sum(fxGs[0 .. sycl::linearize2(gid, lid+sycl::h::warp_sizes(), T/N, N)]);

            resulty = resulty + fx[sycl::linearize2(gid, lidp1, T/N, N)];
            /*@ assert lemmaSumOverConcat(fyGs[0 .. sycl::linearize2(gid, lid, T/N, N)],fyGs[sycl::linearize2(gid, lidp1, T/N, N)-1 .. sycl::linearize2(gid, lidp1, T/N, N)+sycl::h::warp_sizes()-1]);
                assert lemmaSumOverABBCisAC(fyGs,0,sycl::linearize2(gid, lid, T/N, N),sycl::linearize2(gid, lidp1, T/N, N)-1,sycl::linearize2(gid, lidp1, T/N, N)+sycl::h::warp_sizes()-1); */
            /*@ assert intermediateResults2(T,N,sycl::h::warp_sizes(),fx,fxGs,fyGs,fzGs); */
            
            /*@ assert {:2:sycl::linearize2(gid, lidp1, T/N, N):}%4 == 1;
                assert idshiftBack(lid, sycl::h::warp_sizes(), N);
                assert 0 <= lidp1%sycl::h::warp_sizes() - 1;
                assert lidp1%sycl::h::warp_sizes()+sycl::h::warp_sizes()-1 <= sycl::h::warp_sizes(); 
                assert sycl::linearize2(gid, lidp1, T/N, N)+sycl::h::warp_sizes()-1 <= |fyGs|;                           
                assert fx[sycl::linearize2(gid, lidp1, T/N, N)] == sum(fyGs[sycl::linearize2(gid, lidp1, T/N, N)-1 .. sycl::linearize2(gid, lidp1, T/N, N)+sycl::h::warp_sizes()-1]); */

            //@ assert (lid+sycl::h::warp_sizes() < N ) ==> resulty == sum(fyGs[0 .. sycl::linearize2(gid, lid+sycl::h::warp_sizes(), T/N, N)]);

            resultz = resultz + fx[sycl::linearize2(gid, lidp2, T/N, N)];
            /*@ assert lemmaSumOverConcat(fzGs[0 .. sycl::linearize2(gid, lid, T/N, N)],fzGs[sycl::linearize2(gid, lidp2, T/N, N)-2 .. sycl::linearize2(gid, lidp2, T/N, N)+sycl::h::warp_sizes()-2]);
                assert lemmaSumOverABBCisAC(fzGs,0,sycl::linearize2(gid, lid, T/N, N),sycl::linearize2(gid, lidp2, T/N, N)-2,sycl::linearize2(gid, lidp2, T/N, N)+sycl::h::warp_sizes()-2); */
            /*@ assert intermediateResults3(T,N,sycl::h::warp_sizes(),fx,fxGs,fyGs,fzGs); */
            /*@ assert {:3:sycl::linearize2(gid, lidp2, T/N, N):}%4 == 2;
                assert 0 <= lidp2%sycl::h::warp_sizes() - 2;
                assert lidp2%sycl::h::warp_sizes()+sycl::h::warp_sizes()-2 <= sycl::h::warp_sizes(); 
                assert sycl::linearize2(gid, lidp2, T/N, N)+sycl::h::warp_sizes()-2 <= |fzGs|;
                assert fx[sycl::linearize2(gid, lidp2, T/N, N)] == sum(fzGs[sycl::linearize2(gid, lidp2, T/N, N)-2 .. sycl::linearize2(gid, lidp2, T/N, N)+sycl::h::warp_sizes()-2]); */
            
            //@ assert (lid+sycl::h::warp_sizes() < N ) ==> resultz == sum(fzGs[0 .. sycl::linearize2(gid, lid+sycl::h::warp_sizes(), T/N, N)]);

            //@     ghost int lid2=lid+sycl::h::warp_sizes();
            //@     assert (lid2 < N) ==> sycl::linearize2(gid, lid, T/N, N) + sycl::h::warp_sizes() == sycl::linearize2(gid, lid2, T/N, N);
            //@     assert (lid2 == N) ==> sycl::linearize2(gid, lid, T/N, N) + sycl::h::warp_sizes() == sycl::linearize2(gid, lid2-sycl::h::warp_sizes(), T/N, N)+sycl::h::warp_sizes();
            
            /*@ assert idshift(T,N,gid,lid,sycl::h::warp_sizes()); */
            //@ assert lidshift(lid,N,sycl::h::warp_sizes());
            /*@ assert modwrp(lid, sycl::h::warp_sizes()); */
        }
        //@ assert gidshiftPlease(gid, T, N);
    }
    //@ ghost resultxx = resultx;
    //@ ghost resultyy = resulty;
    //@ ghost resultzz = resultz;
    
    //@ assert resultx == sum(fxGs[0 .. T]);
    //@ assert resulty == sum(fyGs[0 .. T]);
    //@ assert resultz == sum(fzGs[0 .. T]);

}