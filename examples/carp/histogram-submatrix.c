// -*- tab-width:2 ; indent-tabs-mode:nil -*-
//:: cases HistogramSubMatrix
// tools silicon
// verdict Pass

/*@
  given seq<seq<int> > data;
  given frac p;
  context_everywhere p!=none && M>0 && N > 0 && P > 0 && step >= N;
  context_everywhere \matrix(matrix,M,N) ** \array(hist,P);
  context_everywhere |data| == M && (\forall int i; 0<=i && i<|data|; |data[i]| == N);

  context (\forall* int i1 ; 0 <= i1 && i1 < M ;
             (\forall* int j1 ; 0 <= j1 && j1 < N ;
               Perm(matrix[i1][j1],p)));
  context (\forall int i1 ; 0 <= i1 && i1 < M ;
             (\forall int j1 ; 0 <= j1 && j1 < N ;
               0 <= matrix[i1][j1] && matrix[i1][j1] < P));
  context (\forall int i1 ; 0 <= i1 && i1 < M ;
             (\forall int j1 ; 0 <= j1 && j1 < N ;
               matrix[i1][j1] == data[i1][j1] ));
  context (\forall* int i1 ; 0 <= i1 && i1 < P ;
               Perm(hist[i1],write));
  ensures  (\forall int k; 0 <= k && k < P ; hist[k]==
               (\sum int i1 ; 0 <= i1 && i1 < M ;
                 (\sum int j1 ; 0 <= j1 && j1 < N ;
                   data[i1][j1]==k?1:0)));
@*/
void histogram(int M,int N,int step,int matrix[M][step],int P,int hist[P]){
  for(int k=0;k<P;k++)
  /*@
    context Perm(hist[k],write);
    ensures hist[k]==0;
  @*/
  {
    hist[k]=0;
  }

  for(int ii=0;ii<M;ii++){
    for(int jj=0;jj<N;jj++)
      /*@
        requires (\forall* int k; 0 <= k && k < P ; Reducible(hist[k],+));
        context  Perm(matrix[ii][jj],p);
        context 0 <= matrix[ii][jj] && matrix[ii][jj] < P ;
        context  matrix[ii][jj]==data[ii][jj];
        ensures  (\forall* int k; 0 <= k && k < P ; Contribution(hist[k],data[ii][jj]==k?1:0));
      @*/
    {
      hist[matrix[ii][jj]]+=1;
    }
  }
}

