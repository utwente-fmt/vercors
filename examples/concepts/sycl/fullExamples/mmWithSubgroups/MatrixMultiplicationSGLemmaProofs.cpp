#include <sycl/sycl.hpp>



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
void lemmaPost(int* c);


ghost 
requires i >= 0;
requires Pf()%tlszf()==0;
ensures (i * Pf()) % tlszf() == 0;
void lemma2(int i) {
    assert true;
    int bla = Pf()/tlszf();
    assert bla*tlszf() == Pf();
    assert (i * bla * tlszf()) % tlszf() == 0;
}

ghost
requires a>=0 && b >=0 &&tlszf()>0;
requires a%tlszf()==0;
ensures (a+b)%tlszf()==b%tlszf();
void lemma123(int a, int b){
    assert true;
    assert a%tlszf()+b%tlszf()==b%tlszf();
    assert (a%tlszf()+b%tlszf())%tlszf() == (a+b)%tlszf();
}

ghost
requires Pf()%tlszf()==0;
requires llid == glid % tlszf();
requires 0 <= glid && glid < Mf()*Pf();
requires 0 <= llid && llid < tlszf();
requires 0 <= k && k < tlszf();
requires 0 <= gid0 && gid0 < Mf();
requires glid/Pf() == gid0;
ensures (glid + k - (llid % tlszf()))/Pf() == gid0;
void lemmaGlobalId(int glid, int llid, int k, int gid0) {
    assert true;
    int rest = glid % Pf();
    assert glid == gid0 * Pf() + rest;
    assert 0 <= rest && rest < Pf();
    assert llid == glid % tlszf();
    assert llid == (gid0 * Pf() + rest) % tlszf();
    assert Pf() % tlszf() == 0;
    assert Pf() == (Pf() / tlszf()) * tlszf();
    ghost lemma2(gid0);
    assert (gid0 * Pf()) % tlszf() == 0;
    assert llid == (gid0 * Pf() + rest) % tlszf();
    ghost lemma123(gid0 * Pf(), rest);
    assert ((gid0 * Pf()) + rest) % tlszf() == rest % tlszf();
    assert llid == (gid0 * Pf() + rest) % tlszf() ==> (gid0 * Pf()) % tlszf() == 0 ==> (gid0 * Pf() + rest) % tlszf() == rest % tlszf();
    assert llid == rest % tlszf();
    assert 0 <= llid && llid < tlszf();
    int shifted = glid + k - llid;
    assert shifted == gid0 * Pf() + rest + k - llid;
    assert shifted == gid0 * Pf() + (rest - llid + k);
    int d = rest - llid + k;
    int q = rest / tlszf();
    assert rest == q * tlszf() + llid;
    assert d == (q * tlszf() + llid) - llid + k;
    assert d == q * tlszf() + k;
    assert 0 <= k && k < tlszf();
    assert d >= q * tlszf();
    assert d <= q * tlszf() + (tlszf() - 1);
    assert q * tlszf() <= Pf() - tlszf();
    assert d < Pf();
    assert d >= 0;
    assert shifted == gid0 * Pf() + d;
    assert (gid0 * Pf() + d) / Pf() == gid0;
    assert shifted / Pf() == gid0;
}

ghost
requires Pf()%tlszf() == 0;
requires 0 <= glid && glid < Mf()*Pf();
requires 0 <= llid && llid < 1*tlszf();
requires 0 <= g0 && g0 < Mf();
requires 0 <= g1 && g1 < Pf();
requires 0 <= l0 && l0 < 1;
requires 0 <= l1 && l1 < tlszf();
requires llid == l0*1+l1;
requires glid == g0*Pf() + g1;
requires l1 == g1%tlszf();
ensures llid == glid%tlszf();
void lemma1234(int glid, int llid, int g0, int g1, int l0, int l1) {
    assert true;
    assert l0 == 0;
    assert llid == l1;
    assert llid == g1 % tlszf();
    ghost lemma2(g0);
    assert (g0 * Pf()) % tlszf() == 0;
    assert glid == g0 * Pf() + g1;
    assert glid % tlszf()
        == ((g0 * Pf()) + g1) % tlszf();
    ghost lemma123(g0 * Pf(), g1);
    // assert ((g0 * Pf()) + g1) % tlszf()
    //     == ((g0 * Pf()) % tlszf() + g1 % tlszf()) % tlszf();
    assert ((g0 * Pf()) % tlszf() + g1 % tlszf()) % tlszf()
        == (0 + g1 % tlszf()) % tlszf();
    assert glid % tlszf() == g1 % tlszf();
    assert llid == glid % tlszf();
}




ghost
requires Pf()%tlszf() == 0;
requires 0 <= g1 && g1 < Pf();
requires 0 <= l1 && l1 < tlszf();
requires g1 == gr1*tlszf()+l1;
ensures g1%tlszf() == l1;
void lemma12334(int g1, int gr1, int l1) {
    assert true;
    assert tlszf() > 0;
    assert g1 == (g1 / tlszf()) * tlszf() + g1 % tlszf();
    assert g1 == gr1 * tlszf() + l1;
    assert 0 <= l1 && l1 < tlszf();
    // uniqueness of quotient/remainder implies:
    assert g1 % tlszf() == l1;
}

ghost 
requires Pf()%tlszf() == 0;
requires 0 <= glid && glid < Mf()*Pf();
requires 0 <= g0 && g0 < Mf();
requires 0 <= g1 && g1 < Pf();
requires glid == g0*Pf() + g1;
ensures g0 == glid/Pf();
void lemma1234445(int glid, int g0, int g1) {
    assert true;
}
@*/




