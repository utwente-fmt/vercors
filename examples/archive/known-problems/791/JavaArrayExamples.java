// -*- tab-width:2 ; indent-tabs-mode:nil -*-
//:: cases JavaArrayExamples
//:: suite puptol
//:: tools silicon
//:: verdict Pass

/*
  Answer to question 4 of the FMSE exam on April 16, 2015.
*/
class Exercise4 {
  
  /*@
     context_everywhere a!=null && b != null && res != null;
     context_everywhere a.length==b.length && res.length==2*a.length;
     context_everywhere (\forall* int i ; 0 <= i && i < a.length ; Perm(a[i],1));
     context_everywhere (\forall* int i ; 0 <= i && i < b.length ; Perm(b[i],1));
     context_everywhere (\forall* int i ; 0 <= i && i < res.length ; Perm(res[i],1));
     ensures (\forall int i ; 0 <= i && i < a.length ; res[2*i]==a[i] );
     ensures (\forall int i ; 0 <= i && i < b.length ; res[2*i+1]==b[i] );
   */
  void zip(int[] a, int[] b, int[] res) {
    int k = 0;
    /*@
      loop_invariant 0 <= k ** k%2 ==0 ** k <= res.length;
      loop_invariant (\forall int i ; 0 <= i && i < k/2 ; res[2*i]==a[i] );
      loop_invariant (\forall int i ; 0 <= i && i < k/2 ; res[2*i+1]==b[i] );      
     */
    while (k < res.length) {
      res[k] = a[k/2];
      res[k+1] = b[k/2];
      k = k+2;
    }
  }
}

class JavaArrayExamples {
  /*@
    context_everywhere a != null;
    context (\forall* int i ; 0 <= i && i < a.length ; Perm(a[i],write));
  @*/
  public void shift_drf(int a[]){
    for(int i=0;i<a.length;i++)
    /*@
      requires Perm(a[i],1\2);
      requires i==0 ==> Perm(a[i],1\2);
      requires i < a.length-1 ==> Perm(a[i+1],1\2);
      ensures  Perm(a[i],write);
    @*/
    {
      //@ recv S;
      if (i < a.length-1) {
        a[i]=a[i+1];
      }
      //@ send S, 1: 0 <= i ** i < a.length - 1 ** Perm(a[i+1],1\2);
    }
  }
  
  /*@ 
    context_everywhere ar!=null;
    context_everywhere (\forall* int k ; 0 <= k && k < ar.length ; Perm(ar[k],1));
    ensures (\forall int k ; 0 <= k && k < ar.length ; ar[k]==0 ) ;
  @*/
  public void zero_array(int ar[]){
    int i=0;
    int N=ar.length;
    //@ loop_invariant 0<= i && i<=N && N==ar.length ;
    //@ loop_invariant (\forall int k ; 0 <= k && k < i ; ar[k]==0 ) ;
    while(i<N){
      ar[i]=0;
      i++;
    }
  }
	
  /*@ 
      context_everywhere ar != null ** M>0 ** N > 0 ** \ndlength(M, N) == ar.length;
      context   (\forall* int k ; 0 <= k && k < ar.length ; Perm(ar[k],write));
      ensures   (\forall  int k ; 0 <= k && k < ar.length ; ar[k]==0 ) ;
   */
  public void zero_array_nested(int ar[],int M,int N){
    for(int i=0;i<M;i++)
    /*@
      context (\forall* int k ; \ndindex(i, M, 0, N) <= k && k < \ndindex(i+1, M, 0, N); Perm(ar[k],write));
      ensures (\forall  int k ; \ndindex(i, M, 0, N) <= k && k < \ndindex(i+1, M, 0, N); ar[k]==0 ) ;
    @*/
    {
      for(int j=0;j<N;j++)
      /*@
		context 0 <= i && i < M;
        context Perm(ar[\ndindex(i, M, j, N)],write);
        ensures ar[\ndindex(i, M, j, N)]==0;
      @*/
      {
        ar[i*N+j]=0;
      }
    }
  }

  /*@ 
    context_everywhere ar != null ** M>0 ** N > 0 ** \ndlength(M, N) == ar.length;
    context (\forall* int k ; 0 <= k && k < ar.length ; Perm(ar[k],write));
    ensures (\forall  int k ; 0 <= k && k < ar.length ; ar[k]==0 ) ;
  @*/
  public void zero_array_smart_nested(int ar[],int M,int N) {
    for(int i=0;i<M;i++)
    /*@
      context (\forall* int j=0 .. N; Perm(ar[\ndindex(i, M, j, N)], write));
      ensures (\forall int i1=0 .. i, int j=0 .. N; ar[\ndindex(i1, M, j, N)] == 0);
    @*/
    {
      for(int j=0;j<N;j++)
      /*@
        context Perm(ar[\ndindex(i, M, j, N)],write);
        ensures ar[\ndindex(i, M, j, N)]==0;
      @*/
      {
        ar[i*N+j]=0;
      }
    }
  }

}

