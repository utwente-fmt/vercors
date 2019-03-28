// -*- tab-width:2 ; indent-tabs-mode:nil -*-
//:: cases basic-examples-c
//:: suite puptol
//:: tools silicon
/*
  This file shows how arrays and matrices can be initialized to 0
  and/or copied.
  
  There are also various failing mutants in this directory, but
  those are on per file for testing purposes.
*/

/*@
  invariant matrix != NULL && M > 0 && N > 0;
	invariant \length(matrix) == M * N;
  context  (\forall* int i1 ; 0 <= i1 && i1 < M ;
             matrix[i1] != NULL ** 
             (\forall* int j1 ; 0 <= j1 && j1 < N ;
               Perm(matrix[i1][j1],write)));
  ensures  (\forall int i3 ; 0 <= i3 && i3 < M ;
             (\forall int j3 ; 0 <= j3 && j3 < N ;
               matrix[i3][j3]==0));
@*/
void zero_matrix(int M,int N,int matrix[M][N]){
  for(int i=0;i<M;i++){
    for(int j=0;j<N;j++)
      /*@
        context matrix[i] != NULL;
        context Perm(matrix[i][j],write);
        ensures matrix[i][j]==0;
      @*/
    {
      matrix[i][j]=0;
    }
  }
}

/*@
  invariant ar!=NULL && \length(ar)==len;
  context (\forall* int i;0<=i && i < len;Perm(ar[i],write));
  ensures (\forall int k ; 0 <= k && k < len ; ar[k]==0 );
@*/
void zero_array(int ar[],int len){
  //@ assert \length(ar)==len ;
  for(int i=0;i < len;i++)
    /*@ 
      context Perm(ar[i],write);
      ensures ar[i]==0;
    @*/
  {
      ar[i]=0;
  }
}

/*@
  invariant a != NULL ** \length(a)==len;
  context   (\forall* int i ; 0 <= i && i < len ; Perm(a[i],write));
  ensures   (\forall int i ; 0 <= i && i < len ; a[i]==\old(a[i]));
@*/
void copy_array(int a[],int len){  
  for(int i=0;i < len;i++)
   /*@
    context Perm(a[i],write);
    ensures a[i]==\old(a[i]);
   @*/
    {
      a[i]=a[i];
    }
}

/*@
  invariant  a!=NULL && b !=NULL && c!=NULL;
  invariant \length(a)==len && \length(b)==len && \length(c)==len;

  context (\forall* int i ; 0 <= i && i < len ; Perm(a[i],write));
  context (\forall* int i ; 0 <= i && i < len ; Perm(b[i],1/2));
  context (\forall* int i ; 0 <= i && i < len ; Perm(c[i],1/2));

  ensures (\forall int i ; 0 <= i && i < len ; a[i]==b[i]+c[i]);
  ensures (\forall int i ; 0 <= i && i < len ; b[i]==\old(b[i]));
  ensures (\forall int i ; 0 <= i && i < len ; c[i]==\old(c[i]));
@*/
void vector_add(int a[],int b[],int c[],int len){  
  for(int i=0;i < len;i++)
  /*@
    context Perm(a[i],write) ** Perm(b[i],1/2) ** Perm(c[i],1/2);
    ensures b[i]==\old(b[i]) ** c[i]==\old(c[i]) ** a[i]==b[i]+c[i];
  @*/
  {
    a[i]=b[i]+c[i];
  }
}


/*@
  invariant  a!=NULL && b !=NULL && c!=NULL;
  invariant \length(a)==N && \length(b)==N && \length(c)==N;

  context (\forall* int i ; 0 <= i && i < N ; Perm(a[i],write));
  context (\forall* int i ; 0 <= i && i < N ; Perm(c[i],write));
  context (\forall* int i ; 0 <= i && i < N ; Perm(b[i],1/2));
@*/
void indep_drf(int a[],int b[],int c[],int N){  
  for(int i=0;i<N;i++) /*@
    context Perm(a[i],write) ** Perm(c[i],write) ** Perm(b[i],1/2);
  @*/ {
    a[i] = b[i] + 1;  
    c[i] = a[i] + 2;
  }
}

/*@
  invariant  a!=NULL && b !=NULL && c!=NULL;
  invariant \length(a)==N && \length(b)==N && \length(c)==N;

  context (\forall* int i ; 0 <= i && i < N ; Perm(a[i],write));
  context (\forall* int i ; 0 <= i && i < N ; Perm(c[i],write));
  context (\forall* int i ; 0 <= i && i < N ; Perm(b[i],1/2));
@*/
void forward_drf(int a[],int b[],int c[],int N){  
  for(int i=0;i < N;i++) /*@
    requires Perm(a[i],write) ** Perm(b[i],1/2) ** Perm(c[i],write);
    ensures  Perm(a[i],1/2) ** Perm(b[i],1/2) ** Perm(c[i],write);          
    ensures  (i>0 ==> Perm(a[i-1],1/2)) ** (i==N-1 ==> Perm(a[i],1/2));
  @*/ {
    a[i]=b[i]+1;
    /*@
      S1:if (i< N-1) {
        send 0 <= i ** i < N - 1 ** Perm(a[i],1/2) to S2,1;
      }
    @*/
    S2:if (i>0) {
      //@ recv 0 < i ** i < N ** Perm(a[i-1],1/2) from S1,1;
      c[i]=a[i-1]+2;
    }
  }
}

/*@
  invariant a!=NULL && b !=NULL && c!=NULL;
  invariant \length(a)==len && \length(b)==len && \length(c)==len;

  context  Perm(a[*],write) ** Perm(b[*],1/2) ** Perm(c[*],write);
  context  (\forall int i; 0 <= i && i < len ; b[i] == i);
  
  ensures  (\forall int i; 0 <= i && i < len ; a[i] == i+1);
  ensures  (\forall int i; 0  < i && i < len ; c[i] == i+2);
@*/
void forward_full(int a[],int b[],int c[],int len){  
  for(int i=0;i < len;i++) /*@
    requires Perm(a[i],write) ** Perm(b[i],1/2) ** Perm(c[i],write);

    requires b[i]==i;

    ensures  Perm(a[i],1/2) ** Perm(b[i],1/2) ** Perm(c[i],write);          
    ensures  i>0 ==> Perm(a[i-1],1/2);
    ensures  i==\length(a)-1 ==> Perm(a[i],1/2);
    
    ensures  a[i]==i+1 && b[i]==i && (i>0 ==> c[i]==i+2);
  @*/ {
    a[i]=b[i]+1;
    /*@
      FS1:if (i< len-1) {
        send 0 <= i ** i < len - 1 ** Perm(a[i],1/2) ** a[i]==i+1 to FS2,1;
      }
    @*/
    FS2:if (i>0) {
      //@ recv 0 < i ** i < len ** Perm(a[i-1],1/2) ** a[i-1]==i from FS1,1;
      c[i]=a[i-1]+2;
    }
  }
}


/*@
  invariant  a!=NULL && b !=NULL && c!=NULL;
  invariant \length(a)==N && \length(b)==N && \length(c)==N;

  context (\forall* int i ; 0 <= i && i < N ; Perm(a[i],write));
  context (\forall* int i ; 0 <= i && i < N ; Perm(b[i],1/2));
  context (\forall* int i ; 0 <= i && i < N ; Perm(c[i],write));
@*/
void backward_drf(int a[],int b[],int c[],int N){  
  for(int i=0;i < N;i++)
   /*@
    requires Perm(a[i],1/2) ** Perm(b[i],1/2) ** Perm(c[i],write);
    requires (i==0 ==> Perm(a[i],1/2)) ** (i < N-1 ==> Perm(a[i+1],1/2));
    ensures  Perm(a[i],1/2) ** Perm(a[i],1/2) ** Perm(b[i],1/2) ** Perm(c[i],write);
   @*/
    {
    /*@
      T1:if (i>0) {
        recv 0 < i ** i < N ** Perm(a[i],1/2) from T2,1;
      }
    @*/
    a[i]=b[i]+1;
    T2:if (i < N-1) {
      c[i]=a[i+1]+2;
      //@ send 0 <= i ** i < N - 1 ** Perm(a[i+1],1/2) to T1,1;
    }
  }
}

/*@
  invariant  a!=NULL && b !=NULL && c!=NULL;
  invariant \length(a)==len && \length(b)==len && \length(c)==len;
  
  context (\forall* int i ; 0 <= i && i < len ; Perm(a[i],write));
  context (\forall* int i ; 0 <= i && i < len ; Perm(b[i],1/2));
  context (\forall* int i ; 0 <= i && i < len ; Perm(c[i],write));

  requires (\forall int tid; 0 <= tid && tid < len ; a [ tid ] == 0);
  requires (\forall int tid; 0 <= tid && tid < len ; b [ tid ] == tid);
 
  ensures  (\forall int i; 0 <= i && i < len ;  a[i] == i+1);
  ensures  (\forall int i; 0 <= i && i < len ;  b[i] == i  );
  ensures  (\forall int i; 0 <= i && i < len-1 ;  c[i] == 2  );
@*/
void backward_full(int a[],int b[],int c[],int len){  
  for(int i=0;i < len;i++)
   /*@
    requires Perm(a[i],1/2);
    requires i==0 ==> Perm(a[i],1/2);
    requires i < len-1 ==> Perm(a[i+1],1/2);
    context  Perm(b[i],1/2);
    context  Perm(c[i],write);
    requires i < len-1 ==> a[i+1]==0;
    context  b[i]==i;

    ensures  Perm(a[i],write);      
    ensures  a[i]==i+1;
    ensures  i < len-1 ==> c[i]==2;
   @*/
    {
    /*@
      FT1:if (i>0) {
        recv 0 < i ** i < len ** i == (i-1)+1 ** \length(a)==len ** Perm(a[i],1/2) from FT2,1;
      }
    @*/
    a[i]=b[i]+1;
    FT2:if (i < len-1) {
      c[i]=a[i+1]+2;
      //@ send 0 <= i ** i < len - 1 ** \length(a)==len ** Perm(a[i+1],1/2) to FT1,1;
    }
  }
}

