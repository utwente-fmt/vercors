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




/*@
	context M == Mf() && N == Nf() && P == Pf() && tlsz == tlszf() && N%tlsz==0 && P%tlsz==0;
	context a != b && b != c && a != c;
	context \pointer(a, sycl::h::mul(M,N), write);
	context \pointer(b, sycl::h::mul(N,P), write);
	context \pointer(c, sycl::h::mul(M,P), write);

	context |gsA()| == sycl::h::mul(M,N) && (∀ int i1=0 .. M, int j1=0 .. N; gsA()[sycl::linearize2(i1,j1,M,N)] == a[sycl::linearize2(i1,j1,M,N)]);
	context |gsB()| == sycl::h::mul(N,P) && (∀ int i1=0 .. N, int j1=0 .. P; gsB()[sycl::linearize2(i1,j1,N,P)] == b[sycl::linearize2(i1,j1,N,P)]);

	ensures (∀ int r1=0 .. M, int c1=0 .. P; c[sycl::linearize2(r1,c1,M,P)] == {:1:sumprodArr(a,b,N,r1,c1,M,N,P):}
	);
@*/
void matrixmul(sycl::queue q, int M, int N, int P, int tlsz, int* a, int* b, int* c) {
    {
	sycl::buffer<int, 2> a_buf = sycl::buffer(a, sycl::range<2>(M, N));
    sycl::buffer<int, 2> b_buf = sycl::buffer(b, sycl::range<2>(N, P));
    sycl::buffer<int, 2> c_buf = sycl::buffer(c, sycl::range<2>(M, P));

    sycl::event e2 = q.submit([&](sycl::handler& h) {
        sycl::accessor<int, 2, sycl::access_mode::read> a_acc = sycl::accessor(a_buf, h, sycl::read_only);
        sycl::accessor<int, 2, sycl::access_mode::read> b_acc = sycl::accessor(b_buf, h, sycl::read_only);
        sycl::accessor<int, 2, sycl::access_mode::read_write> c_acc = sycl::accessor(c_buf, h, sycl::read_write);
        h.parallel_for(sycl::nd_range<2>(sycl::range<2>(M,P), sycl::range<2>(1, tlsz)),
            /*@
            context_everywhere M == Mf() && N == Nf() && P == Pf() && tlsz == tlszf() && Nf()%tlszf()==0 && Pf()%tlszf()==0;
            context_everywhere |gsA()| == sycl::h::mul(Mf(),Nf()) && |gsB()| == sycl::h::mul(Nf(),Pf());

            context_everywhere (∀ int i1=0 .. Mf(), int j1=0 .. Nf(); 0 <= {:sycl::linearize2(i1,j1,Mf(),Nf()):} && sycl::linearize2(i1,j1,Mf(),Nf()) < sycl::h::mul(Mf(),Nf()) && sycl::linearize2(i1,j1,Mf(),Nf()) < |gsA()|);
            context_everywhere (∀ int i1=0 .. Nf(), int j1=0 .. Pf(); 0 <= {:sycl::linearize2(i1,j1,Nf(),Pf()):} && sycl::linearize2(i1,j1,Nf(),Pf()) < sycl::h::mul(Nf(),Pf()) && sycl::linearize2(i1,j1,Nf(),Pf()) < |gsB()|);

            context (∀ int i1=0 .. Mf(), int j1=0 .. Nf(); gsA()[{:sycl::linearize2(i1,j1,Mf(),Nf()):}] == a_acc[i1][j1]);
            context (∀ int i1=0 .. Nf(), int j1=0 .. Pf(); gsB()[{:sycl::linearize2(i1,j1,Nf(),Pf()):}] == b_acc[i1][j1]);

            context Perm(c_acc[it.get_global_id(0)][it.get_global_id(1)], write);
            ensures c_acc[it.get_global_id(0)][it.get_global_id(1)] == sumprod(gsA(), gsB(), Nf(), it.get_global_id(0), it.get_global_id(1), Mf(), Nf(), Pf());
            @*/
            [=](sycl::nd_item<2> it) {
                int m = it.get_global_id(0); //lin2(group_id_0, local_id_0, group_range0, local_range0);
                int n = it.get_global_id(1); //lin2(group_id_1, local_id_2, group_range1, local_range1);
                int i = it.get_local_id(1); // 0..tlsz

                int sum = 0;
                /*@ ghost int k1 = 0; */
                /*@
                    loop_invariant (∀ int i1=0 .. Mf(), int j1=0 .. Nf(); gsA()[{:sycl::linearize2(i1,j1,Mf(),Nf()):}] == a_acc[i1][j1]);
                    loop_invariant (∀ int i1=0 .. Nf(), int j1=0 .. Pf(); gsB()[{:sycl::linearize2(i1,j1,Nf(),Pf()):}] == b_acc[i1][j1]);
                    loop_invariant m == it.get_global_id(0) && n == it.get_global_id(1) && i == it.get_local_id(1);
                    loop_invariant 0 <= l && l <= Nf() && 0 <= k1 && l==tlszf()*k1 && l%tlszf()==0;
                    loop_invariant sum == sumprod(gsA(), gsB(), l, it.get_global_id(0), it.get_global_id(1),Mf(),Nf(),Pf());
                */
                for (int l = 0; l < N; l += tlsz) {
                    //@ ghost lemmalStep(l);
                    //@ assert l < N  && l % tlszf() == 0;
                    //@ assert 0 <= i && i < tlszf();

                    //@ assert 0 <= l && l <= N-tlsz;
                    //@ assert l + i < N;
                    int tileA = a_acc[m][l + i];

                    /*@
                        loop_invariant (∀ int i1=0 .. Mf(), int j1=0 .. Nf(); gsA()[{:sycl::linearize2(i1,j1,Mf(),Nf()):}] == a_acc[i1][j1]);
                        loop_invariant (∀ int i1=0 .. Nf(), int j1=0 .. Pf(); gsB()[{:sycl::linearize2(i1,j1,Nf(),Pf()):}] == b_acc[i1][j1]);

                        loop_invariant m == it.get_global_id(0) && n == it.get_global_id(1) && i == it.get_local_id(1);
                        loop_invariant 0 <= it.get_local_id(1) && it.get_local_id(1) < tlszf();
                        loop_invariant M == Mf() && N == Nf() && P == Pf() && tlsz == tlszf() && N%tlsz==0 && P%tlsz==0;
                        loop_invariant 0 <= l && l <= Nf() && 0 <= k1 && l==tlszf()*k1 && l%tlszf()==0;
                        loop_invariant 0 <= k && k <= tlszf();
                        loop_invariant sum == sumprod(gsA(), gsB(), l+k, it.get_global_id(0), it.get_global_id(1),Mf(),Nf(),Pf());
                    */
                    for (int k = 0; k < tlsz; k=k+1) {
                        //@ ghost lemmalStep(l);
//                      int sg_result = group_broadcast(sg, tileA, k);
                      int sg_result = a_acc[m][l + k];
                      sum += sg_result * b_acc[l + k][n];
//                      sum += group_broadcast(sg, tileA, k) * b_acc[l + k][n];

                    }
                    //@ ghost lemmalStep2(l,k1);
                    /*@ ghost k1=k1+1;*/
                }
                c_acc[m][n] = sum;
            });
          });
       e2.wait();
	}
    /*@
        ghost lemmaPost(c);
	    assert (∀ int r1=0 .. M, int c1=0 .. P; 
			lemmaArrSeq(a,gsA(),b,gsB(),N,r1,c1,M,N,P) && 
			c[sycl::linearize2(r1,c1,M,P)] == {:1:sumprodArr(a,b,N,r1,c1,M,N,P):} 
		);
    */
}
































