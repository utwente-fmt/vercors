package vct.test.integration.features

import vct.test.integration.helper.VercorsSpec

class ParBlockSpec extends VercorsSpec {
  vercors should verify using silicon in "example with singleton parallel block" pvl """
    class Test {
      int x;
      requires Perm(x, write);
      void test() {
        par
        requires Perm(x, write); {
          x = 0;
        }
      }
    }
  """

  vercors should verify using silicon in "example with singleton parallel block, but wrong contract" pvl """
    class Test {
      int x;
      requires Perm(x, write);
      void test() {
        /*[/expect parPostFailed:false]*/
        par
        requires Perm(x, write);
        ensures Perm(x, write) ** x == 1;
        {
          x = 0;
        }
        /*[/end]*/
      }
    }
  """

  vercors should verify using silicon in "example with singleton parallel block with insufficient permission" pvl """
    class Test {
      int x;
      requires Perm(x, write);
      void test() {
        par
        requires Perm(x, 1\2); {
          /*[/expect assignFieldFailed]*/
          x = 0;
          /*[/end]*/
        }
      }
    }
  """

  vercors should verify using silicon in "example with nontrivial parallel block" pvl """
    class Test {
      int x;
      requires (∀int i, int j; (0 <= i && i < |xs| && 0 <= j && j < |xs|) ==> (xs[i] == xs[j] ==> i == j));
      requires (∀*int i; 0 <= i && i < |xs| ==> Perm(xs[i].x, write));
      void test(seq<Test> xs) {
        par(int i = 0 .. |xs|)
        context (∀int i, int j; (0 <= i && i < |xs| && 0 <= j && j < |xs|) ==> (xs[i] == xs[j] ==> i == j));
        context Perm(xs[i].x, write);
        ensures xs[i].x == 0;
        {
          xs[i].x = 0;
        }
      }
    }
  """

  vercors should verify using silicon in "example showing parallel blocks are verified non-modularly" pvl """
    requires x > 5;
    void test(int x) {
      par { assert x > 3; }
    }
  """

  vercors should verify using silicon in "example with an array-modifying parallel block, using non-modularly that ar != null" pvl """
    requires ar != null;
    requires Perm(ar[*], write);
    void test(int[] ar) {
      par(int i = 0 .. ar.length)
      context Perm(ar[i], write);
      ensures ar[i] == 0;
      {
        assert ar != null;
        ar[i] = 0;
      }
    }
  """

  vercors should verify using silicon in "example with an array-reading parallel block, showing that par forgets about its heap chunk" pvl """
    requires ar != null;
    requires Perm(ar[*], 1\2);
    requires (\forall int i = 0 .. ar.length; ar[i] == 0);
    void test(int[] ar) {
      par(int i = 0 .. ar.length)
      requires Perm(ar[i], 1\2);
      {
        /*[/expect assertFailed:false]*/
        assert ar[i] == 0;
        /*[/end]*/
      }
    }
  """
}
