package vct.test.integration.features

import vct.test.integration.helper.VercorsSpec

class AmbiguousOps extends VercorsSpec {
  vercors should verify using anyBackend in "example showing use of bag operator in java" java """
    class Test {
      //@ resource mylockset(bag<int> xs);

      //@ given bag<int> x;
      //@ requires mylockset(x);
      void foo() {
       //@ assert mylockset(x);
       //@ [/expect assertFailed:perm] assert mylockset(x - bag<int>{0}); [/end]
      }
    }
  """

  vercors should verify using anyBackend in "example showing collapsed addition to sequence in java" java """
    class T {
      void test() {
        //@ ghost seq<int> xs;
        //@ ghost xs += [1, 2, 3];
      }
    }
  """

  vercors should verify using anyBackend in "example showing collapsed pointer arithmetic" c """
    //@ requires x != NULL ** Perm(x, write) ** \pointer_block_length(x) >= 2 + \pointer_block_offset(x);
    void test(int *x) {
      int *y = x;
      y += 1;
    }
  """
}
