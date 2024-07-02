
/*@
  context_everywhere x0 != NULL ** \pointer_length(x0) == n ** (\forall* int i; 0<=i && i<n; Perm(&x0[i], write));
  context_everywhere x1 != NULL ** \pointer_length(x1) == n ** (\forall* int i; 0<=i && i<n; Perm(&x1[i], 1\2));
  context_everywhere x2 != NULL ** \pointer_length(x2) == n ** (\forall* int i; 0<=i && i<n; Perm(&x2[i], 1\2));
  ensures (\forall int i; 0<=i && i<n; x0[i] == x1[i] + x2[i] );
@*/
int main(int n, /*@ unique<1> @*/ int* x0, /*@ unique<2> @*/ int* x1, /*@ unique<2> @*/ int* x2){
  /*@
    loop_invariant 0 <= j && j <= n;
    loop_invariant (\forall int i; 0<=i && i<j; x0[i] == x1[i] + x2[i] );
  @*/
  for(int j=0; j<n; j++){
    x0[j] = x1[j] + x2[j];
  }
}