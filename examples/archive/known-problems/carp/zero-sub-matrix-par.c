// -*- tab-width:2 ; indent-tabs-mode:nil -*-
//:: cases ZeroSubMatrixPar-C
//:: tools silicon
//:: suite problem-fail
//:: verdict Pass

/*@
  context \pointer(matrix, M, write);
  context (\forall* int i; 0 <= i && i < M;
            (\forall* int j; 0 <= j && j < N; Perm(matrix[i][j], write)));
  context M > 0 && N > 0 && step >= N;
  ensures (\forall int i; 0 <= i && i < M;
            (\forall int j; 0 <= j && j < N; matrix[i][j] == 0));
@*/
void zero(int M,int N,int step,int matrix[M][step]){
    /*@
      loop_invariant \pointer(matrix, M, write);
      loop_invariant (\forall* int i; 0 <= i && i < M;
                (\forall* int j; 0 <= j && j < N; Perm(matrix[i][j], write)));
      loop_invariant 0 <= i && i <= M;
      loop_invariant (\forall int i2; 0 <= i2 && i2 < i; (\forall int j; 0 <= j && j < N;matrix[i2][j] == 0));
    @*/
  for(int i=0;i<M;i++)
  {
      /*
        loop_invariant \pointer(matrix, M, write);
        loop_invariant (\forall* int i; 0 <= i && i < M;
                (\forall* int j; 0 <= j && j < N; Perm(matrix[i][j], write)));
        loop_invariant M > 0 && N > 0 && step >= N && 0 <= j && j <= N;
        loop_invariant (\forall* int j2; 0 <= j2 && j2 < j; matrix[i][j2] == 0);
        loop_invariant (\forall int i2; 0 <= i2 && i2 < i; (\forall int j; 0 <= j && j < N;matrix[i2][j] == 0));
      */
    for(int j=0;j<N;j++)
    /*@
      requires matrix != NULL;
      context Perm(matrix[i],1/N);
      context Perm(matrix[i][j], write);
      ensures matrix[i][j] == 0;
    @*/
    {
      matrix[i][j]=0;
    }
    //@ assert M > 0 && N > 0 && step >= N;
    //@ assert (\forall* int i; 0 <= i && i < M; (\forall* int j; 0 <= j && j < N; Perm(matrix[i][j], write)));
  }
}
