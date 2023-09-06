#include <stdlib.h>
#include <assert.h>

int main () {
   int *xs;

   /* Initial memory allocation */
   xs = (int *) malloc(15 * sizeof(int));
   /*@
    loop_invariant 0 <= i && i <= 15;
    loop_invariant xs != NULL && \pointer_length(xs) == 15;
    loop_invariant (\forall* int j; 0 <= j && j < 15; Perm(&xs[j], write));
    loop_invariant (\forall int j; 0 <= j && j < i; xs[j] == j);
   @*/
   for(int i=0; i<15;i++){
     xs[i] = i;
   }

   assert(xs[0] == 0);

   free(xs);

   // Should fail
   //@ assert(xs[0] == 0);

   return 0;
}