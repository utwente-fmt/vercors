package vct.test.integration.examples

import vct.test.integration.helper.VercorsSpec

class CSpec extends VercorsSpec {
  vercors should verify using silicon example "concepts/c/casts.c"
  vercors should verify using silicon example "concepts/c/floats.c"
  vercors should verify using silicon example "concepts/c/malloc_free.c"
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

  vercors should fail withCode "ptrOffsetNonZero" using silicon in "free null pointer" c
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
}