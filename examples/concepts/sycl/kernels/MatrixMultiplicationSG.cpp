#include <sycl/sycl.hpp>

// ~/Documents/vercors/patch-sycl/bin/vct --no-infer-heap-context-into-frame mm_sycl_1_2_3.cpp  --profile --backend-file-base mm_sycl_1_2_3.cpp
// Takes ~6 minutes

/*@

//////////////////////
/// Helper methods ///
//////////////////////
ensures \result > 0;
pure int Mf();
ensures \result > 0;
pure int Nf();
ensures \result > 0;
pure int Pf();
ensures \result > 1;
pure int tlszf();

pure seq<int> gsA();
pure seq<int> gsB();
pure int decr(int n) = n-1;


////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////// Functions ////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
context M == Mf() && N == Nf() && P == Pf();
requires A != B;
requires \pointer(A, sycl::h::mul(M,N), read);
requires \pointer(B, sycl::h::mul(N,P), read);
context 0 <= r && r < M && 0 <= c && c < P;
context 0 <= j && j <= N;
decreases assume;
pure int sumprodArr(int* A, int*  B, int j, int r, int c, int M, int N, int P) =
	j == 0 ?
		0:
		sycl::h::mul(A[sycl::linearize2(r,decr(j), M, N)],B[sycl::linearize2(decr(j),c,N,P)]) + sumprodArr(A,B,decr(j),r,c,M,N,P);


context M == Mf() && N == Nf() && P == Pf();
requires |A| == sycl::h::mul(M,N);
requires |B| == sycl::h::mul(N,P);
context 0 <= r && r < M && 0 <= c && c < P;
context 0 <= j && j <= N;
decreases assume;
pure int sumprod(seq<int> A, seq<int>  B, int j, int r, int c, int M, int N, int P) =
    j == 0 ?
		0 :
    	sycl::h::mul(A[sycl::linearize2(r,decr(j),M,N)],B[sycl::linearize2(decr(j),c,N,P)]) + sumprod(A,B,decr(j),r,c,M,N,P)
;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////// LEMMAs ///////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

context M == Mf() && N == Nf() && P == Pf();
context |gA| == sycl::h::mul(M,N);
context |gB| == sycl::h::mul(N,P);
requires A != B;
requires \pointer(A, sycl::h::mul(M,N), read) ** \pointer(B, sycl::h::mul(N,P), read);
context 0 <= r && r < M && 0 <= c && c < P && 0 <= j && j <= N;
context (\forall* int i1; 0<=i1&&i1<M; (\forall* int j1; 0<=j1&&j1<N; gA[{:sycl::linearize2(i1,j1,M,N):}] == A[sycl::linearize2(i1,j1,M,N)]));
context (\forall* int i1; 0<=i1&&i1<N; (\forall* int j1; 0<=j1&&j1<P; gB[{:sycl::linearize2(i1,j1,N,P):}] == B[sycl::linearize2(i1,j1,N,P)]));

ensures j == 0 ==> sumprodArr(A, B, j, r, c, M, N, P) == sumprod(gA, gB, j, r, c, M, N, P);
ensures j != 0 ==> sumprodArr(A, B, j, r, c, M, N, P) == sumprod(gA, gB, j, r, c, M, N, P);
ensures \result && sumprodArr(A, B, j, r, c, M, N, P) == sumprod(gA, gB, j, r, c, M, N, P);
pure bool lemmaArrSeq(int* A, seq<int> gA, int*  B, seq<int> gB, int j, int r, int c, int M, int N, int P);


requires 0 <= l && l < Nf() && l%tlszf()==0 && Nf()%tlszf()==0;
ensures 0 <= l && l <= Nf()-tlszf();
ensures \result;
pure bool lemmalStep(int l) = true;

requires 0 <= l && l <= Nf() && l==tlszf()*k1 && l%tlszf()==0;
ensures (l+tlszf())%tlszf()==0 && (l+tlszf())==tlszf()*(k1+1);
ensures \result;
pure bool lemmalStep2(int l, int k1) = true;

ensures \result; 
pure bool tr0(int g0, int g1, int g2, int g3);
ensures \result; 
pure bool tr1(int g0, int g1, int g2, int g3);
ensures \result; 
pure bool tr3(int g0, int g1, int g2, int g3);
ensures \result; 
pure bool tr4(int g0, int g1, int g2, int g3, int g4);
ensures \result; 
pure bool tr5(int g0, int g1, int g2, int g3);

ghost
context \pointer(c, sycl::h::mul(Mf(),Pf()), write);
context |gsA()| == sycl::h::mul(Mf(),Nf());
context |gsB()| == sycl::h::mul(Nf(),Pf());
context Pf()%tlszf() == 0;
requires (\forall int gid02=0 .. Mf(), int lid02=0 .. 1, int gid3=0 .. Pf()/tlszf(), int lid3=0 .. tlszf();
    {:c[sycl::linearize2(sycl::linearize2(gid02,lid02, Mf(), 1),sycl::linearize2(gid3, lid3,Pf()/tlszf(), tlszf()), Mf(),Pf())]:}
    ==
    sumprod(gsA(), gsB(), Nf(), sycl::linearize2(gid02,lid02, Mf(), 1), sycl::linearize2(gid3, lid3,Pf()/tlszf(), tlszf()), Mf(),Nf(), Pf())
);
ensures (\forall int gid02=0 .. Mf(), int gl3=0 .. Pf();
    c[gid02*Pf()+(gl3)] == {:sumprod(gsA(), gsB(), Nf(), gid02, gl3, Mf(),Nf(), Pf()):}
);
void lemmaPost(int* c) {
    assert (\forall int gid02=0 .. Mf(), int lid02=0 .. 1, int gid3=0 .. Pf()/tlszf(), int lid3=0 .. tlszf(); {:tr0(gid02, lid02, gid3, lid3):} ==> 
        c[sycl::linearize2(sycl::linearize2(gid02,lid02, Mf(), 1),sycl::linearize2(gid3, lid3,Pf()/tlszf(), tlszf()), Mf(),Pf())]
        ==
        c[gid02*Pf()+gid3*tlszf()+lid3]
    ); 

    assert (\forall int gid02=0 .. Mf(), int lid02=0 .. 1, int gid3=0 .. Pf()/tlszf(), int lid3=0 .. tlszf(); {:tr1(gid02, lid02, gid3, lid3):} ==> 
        sumprod(gsA(), gsB(), Nf(), sycl::linearize2(gid02,lid02, Mf(), 1), sycl::linearize2(gid3, lid3,Pf()/tlszf(), tlszf()), Mf(),Nf(), Pf())
        ==
        sumprod(gsA(), gsB(), Nf(), gid02, gid3*tlszf()+lid3, Mf(),Nf(), Pf())
    );

    assert (\forall int gid02=0 .. Mf(), int lid02=0 .. 1, int gid3=0 .. Pf()/tlszf(), int lid3=0 .. tlszf();  tr0(gid02, lid02, gid3, lid3) ==> tr1(gid02, lid02, gid3, lid3) ==>
        c[gid02*Pf()+gid3*tlszf()+lid3]
        ==
        sumprod(gsA(), gsB(), Nf(), gid02, gid3*tlszf()+lid3, Mf(),Nf(), Pf())
    );


    assert (\forall int gid02=0 .. Mf(); (\forall int lid02=0 .. 1, int gid3=0 .. Pf()/tlszf(), int lid3=0 .. tlszf();  
        tr0(gid02, lid02, gid3, lid3) ==> tr1(gid02, lid02, gid3, lid3) ==>
        c[gid02*Pf()+gid3*tlszf()+lid3]
        ==
        {:sumprod(gsA(), gsB(), Nf(), gid02, gid3*tlszf()+lid3, Mf(),Nf(), Pf()):}
    ));

    assert Pf() == Pf()/tlszf()*tlszf();
    
    assert (\forall int gid02=0 .. Mf(), int gl3=0 .. Pf();
        tr0(gid02, 0, gl3/tlszf(), gl3%tlszf()) ==> tr1(gid02, 0, gl3/tlszf(), gl3%tlszf()) ==>
        c[gid02*Pf()+gl3] == sumprod(gsA(), gsB(), Nf(), gid02, gl3, Mf(),Nf(), Pf())
    );

}
@*/

// assert (forall int g2=0..Mf(), int gl3=0..tlszf1() * truncdiv(pf1(), tlszf1()); tr8(g2, 0, gl3 \ tlszf1(), gl3 % tlszf1()) && tr01(g2, 0, gl3 \ tlszf1(), gl3 % tlszf1()) && tr2(g2, 0, gl3 \ tlszf1(), gl3 % tlszf1()) ==>
//     ptrDeref(ptrAdd(optGet1(c), g2 * pf1() + gl3 \ tlszf1() * tlszf1() + gl3 % tlszf1())).int == sumprod1(gsA1(), gsB1(), nf1(), g2, gl3, mf1(), nf1(), pf1())))
// assert (\forall int g2=0..Mf(), int gl3=0..Pf(); tr8(g2, 0, truncdiv(gl3, tlszf1()), truncmod(gl3, tlszf1())) ==>
//     ptrDeref(ptrAdd(optGet1(c), g2 * pf1() + gl3)).int == sumprod1(gsA1(), gsB1(), nf1(), g2, gl3, mf1(), nf1(), pf1()))


// ptrDeref(ptrAdd(optGet1(c), g2 * pf1() + gl3 \ tlszf1() * tlszf1() + gl3 % tlszf1())).int == sumprod1(gsA1(), gsB1(), nf1(), g2, gl3, mf1(), nf1(), pf1())))
// ptrDeref(ptrAdd(optGet1(c), g2 * pf1() + gl3                                       )).int == sumprod1(gsA1(), gsB1(), nf1(), g2, gl3, mf1(), nf1(), pf1()))