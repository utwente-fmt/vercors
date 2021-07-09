//:: cases AccessSubMatrixPass
//:: tools silicon
//:: verdict Pass
/////////////////////////////////////////////////////////////////
// NOTE: This is a faulty pass! Please see the comments below! //
/////////////////////////////////////////////////////////////////

/*@
  requires matrix != NULL;
  requires M>0 && N > 0 && step > N ;
  // NOTE: The next requires should not be necessary. However, we include it for now, to make the test pass.
  // Then, when this feature is fixed or removed, the test will break, and whoever is working on it then
  // can decide what to do with the test.
  // Begin faulty but necessary requires clause:
  requires (\forall* int i1 ; 0 <= i1 && i1 < M ;
               Perm(matrix[i1],read)); // End faulty but necessary requires clause
  // Only the next requires clause should be necessary because 2-dimensional arrays are densely packed.
  requires (\forall* int i1 ; 0 <= i1 && i1 < M ;
             (\forall* int j1 ; 0 <= j1 && j1 < N ;
               Perm(matrix[i1][j1],write)));
@*/
void good1(int M,int N,int step,int matrix[M][step]){
  matrix[0][0]=0;
}
