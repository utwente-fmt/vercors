package vct.test.integration.examples

import vct.test.integration.helper.VercorsSpec

class CSpec extends VercorsSpec {
  vercors should verify using silicon example "concepts/c/casts.c"
  vercors should verify using silicon example "concepts/c/floats.c"
  vercors should verify using silicon example "concepts/c/malloc_free.c"
  vercors should verify using silicon example "concepts/c/math.c"
  vercors should verify using silicon example "concepts/c/mod_div.c"
  vercors should verify using silicon example "concepts/c/structs.c"

  vercors should error withCode "resolutionError:type" in "float should not be demoted" c
  """
  int main(){
    int x = 4.0 % 1;
  }
  """
  vercors should fail withCode "assignFieldFailed" using silicon in "cannot access field of struct after freeing" c
    """
    #include <stdlib.h>

    struct d{
      int x;
    };

    struct e{
      struct d s;
      int x;
    };

    int main(){
      struct e* a = (struct e*) malloc(1*sizeof(struct e));
      a->s.x = 1;
      struct d* b = &(a->s);
      free(a);
      b->x = 2;
    }
    """

  vercors should fail withCode "ptrNull" using silicon in "free null pointer" c
    """
      #include <stdlib.h>
      int main(){
          int* xs;
          free(xs);
      }
    """

  vercors should fail withCode "ptrOffsetNonZero" using silicon in "free offset 1 pointer" c
    """
      #include <stdlib.h>
      int main(){
          int* xs = (int*) malloc(sizeof(int)*3);
          free(xs+1);
      }
    """

  vercors should fail withCode "ptrFreePerm" using silicon in "free pointer with insufficient permission" c
    """
      #include <stdlib.h>
      int main(){
          int* xs = (int*) malloc(sizeof(int)*3);
          //@ exhale Perm(&xs[0], 1\2);
          free(xs);
      }
    """
  vercors should fail withCode "ptrFreeFieldError" using silicon in "free pointer with insufficient permission for field" c
    """
    #include <stdlib.h>
    struct d{
      int x;
    };
    int main(){
      struct d* xs = (struct d*) malloc(sizeof(struct d)*3);
      struct d* ys = (struct d*) malloc(sizeof(struct d)*3);
      //@ exhale Perm(xs[0].x, 1\2);
      free(xs);
    }
    """
  vercors should fail withCode "ptrNull" using silicon in "Deref field of null ptr" c
    """
    struct d{
      int x;
    };
    int main(){
      struct d* s;
      s->x = 1;
    }
    """

  vercors should fail withCode "ptrPerm" using silicon in "Deref field of zero perm ptr" c
    """
    struct d{
      int x;
    };
    int main(){
      struct d s1;
      struct d* s2 = &s1;
      //@ exhale Perm(s2, 1\1);
      s2->x = 1;
    }
    """

  vercors should fail withCode "assignFieldFailed" using silicon in "Deref field of zero perm field" c
    """
    struct d{
      int x;
    };
    int main(){
      struct d s1;
      struct d* s2 = &s1;
      //@ exhale Perm(s2->x, 1\1);
      s2->x = 1;
    }
    """

  vercors should fail withCode "assignFieldFailed" using silicon in "Access field of zero perm ptr" c
    """
    struct d{
      int x;
    };
    int main(){
      struct d s;
      //@ exhale Perm(s.x, 1\1);
      s.x = 1;
    }
    """
  vercors should fail withCode "perm" using silicon in "Read field of zero perm ptr" c
    """
    struct d{
      int x;
    };
    int main(){
      struct d s;
      s.x = 1;
      //@ exhale Perm(s.x, 1\1);
      int x = s.x;
    }
    """

  vercors should error withCode "unsupportedCast" in "Cast ptr struct to int" c
    """
    struct d{
      int x;
    };
    int main(){
      struct d *s;
      int* ss;
      ss = (int *) s;
    }
    """

  vercors should error withCode "unsupportedCast" in "Cast struct to int" c
    """
    struct d{
      int x;
    };
    int main(){
      struct d s;
      int ss;
      ss = (int ) s;
    }
    """

  vercors should error withCode "unsupportedCast" in "Cast int to struct" c
    """
    struct d{
      int x;
    };
    int main(){
      struct d s;
      int ss = 5;
      s = (struct d) ss;
    }
    """

  vercors should error withCode "unsupportedMalloc" in "Unsupported malloc without sizeof" c
    """
    #include <stdlib.h>
    int main(){
      int *x = (int*) malloc(5*4);
    }
    """

  vercors should error withCode "unsupportedMalloc" in "Unsupported malloc with wrong cast" c
    """
    #include <stdlib.h>
    int main(){
      float *x = (float* ) malloc(sizeof(int)*4);
    }
    """

  vercors should error withCode "unsupportedSizeof" in "Unsupported use of sizeof" c
    """
    #include <stdlib.h>
    int main(){
      int x = sizeof(int);
    }
    """

  vercors should fail withCode "divByZero" using silicon in "Truncated div divide zero" c
    """
    int test(int a, int b){
      return a/b;
    }
    """

  vercors should fail withCode "divByZero" using silicon in "Truncated mod divide zero" c
    """
    int test(int a, int b){
      return a%b;
    }
    """

  vercors should fail withCode "divByZero" using silicon in "Eucl div divide zero" c
    """
    int test(int a, int b){
      return a/b;
    }
    """

  vercors should fail withCode "divByZero" using silicon in "Eucl mod divide zero" c
    """
    int test(int a, int b){
      return a%b;
    }
    """

  vercors should error withCode "noSuchName" in "No struct found" c
    """
    struct d {
        int x;
    };

    int main(){
        struct y x;
    }
    """

  vercors should verify using silicon in "Pure function in c" c
    """
    #include <stdlib.h>
    #include <assert.h>

    /*@ pure @*/ int plusOne(int x){
        return x+1;
    }

    //@ ensures \result == plusOne(x);
    int test(int x){
        return x+1;
    }

    int main(){
        assert(test(1) == 2);
    }
    """
  vercors should error withCode "unsupportedStructPerm" in "cylic struct" c
    """
    struct d {
      int x;
      struct d y;
    };

    //@ requires Perm(s, write);
    void test (struct d s) {
      int x;
    }

    int main () {
      struct d s;
    }
    """

  vercors should error withCode "notAValue" in "struct type is no value" c
    """
    struct d {
        int x;
    };

    int main(){
        struct d s = d;
    }
    """

  vercors should error withCode "resolutionError:type" in "struct type is again no value" c
    """
    struct d {
        int x;
    };

    int main(){
        struct d s;
        s = d;
    }
    """

  vercors should error withCode "typeUsedAsValue" in "Struct deref type is used as value" c
    """
    struct d {
        int x;
    };

    int main(){
        struct d s;
        //@ exhale Perm(d.x, 1\1);
    }
    """

  vercors should fail withCode "copyStructFailedBeforeCall" using silicon in "Insufficient permission for field x to copy struct before call" c
    """
    struct d {
        int x;
    };


    int test(struct d t){
        return 1;
    }

    int main(){
        struct d s;
        //@ exhale Perm(s.x, 1\1);
        test(s);
    }
    """

  vercors should fail withCode "copyStructFailed" using silicon in "Insufficient permission for field x to copy struct" c
    """
    struct d {
        int x;
    };


    int test(struct d t){
        return 1;
    }

    int main(){
        struct d s, t;
        //@ exhale Perm(s.x, 1\1);
        t = s;
    }
    """

    vercors should verify using silicon in "Parallel omp loop with declarations inside" c
    """
    #include <omp.h>
    #include <assert.h>

    int main(){
        int sum[3] = {0, 0, 0};

        #pragma omp parallel for
        for(int i=0;i<3;i++)
        /*@
            context 0 <= i && i <3;
            context Perm(&sum[i], write);
            requires sum[i] == 0;
            ensures sum[i] == i;
        @*/
        {
            int xs[1] = {i};
            sum[i] += xs[0];
        }
        assert(sum[0] == 0);
        assert(sum[1] == 1);
        assert(sum[2] == 2);
    }
    """

    vercors should verify using silicon in "Casting null to pointers" c
    """
    #include <stdlib.h>

    struct nested {
      struct nested *inner;  
    };

    void main() {
      int *ip = NULL;                              
      double *dp = NULL;                           
      struct nested *np = NULL;                    
      // np = (struct nested*) NULL;               
      np = (struct nested*) malloc(sizeof(struct nested));
      // np->inner = NULL;
      // np->inner = (struct nested*) NULL;         
    }
    """
}