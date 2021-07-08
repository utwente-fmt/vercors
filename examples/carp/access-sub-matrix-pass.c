//:: cases AccessSubMatrixPass
//:: tools silicon
//:: verdict Pass

/*@
  requires matrix != NULL;
  requires M>0 && N > 0 && step > N ;
  requires (\forall* int i1 ; 0 <= i1 && i1 < M ;
             (\forall* int j1 ; 0 <= j1 && j1 < N ;
               Perm(matrix[i1][j1],write)));
@*/
void good1(int M,int N,int step,int matrix[M][step]){
  matrix[0][0]=0;
}
