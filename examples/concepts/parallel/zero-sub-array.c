// -*- tab-width:2 ; indent-tabs-mode:nil -*-
//:: cases ZeroSubArray-C
//:: tools silicon
//:: verdict Pass

/*@
  context \pointer(matrix, M, write);
  context M > 0;
  ensures (\forall int i; 0 <= i && i < M; matrix[i] == 0);
@*/
void zero(int M, int matrix[M]){
  for(int i=0; i<M; i++)
  /*@
    requires matrix != NULL;
    context Perm(matrix + i, write);
    ensures matrix[i] == 0;
  @*/
  {
      matrix[i] = 0;
  }
}
