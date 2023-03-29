package vct.test.integration.features

import vct.test.integration.helper.VercorsSpec

class JavaLiteralArraySpec extends VercorsSpec {
  vercors should verify using silicon in "example with literal java array value" java """
    class Test {
      void test() {
        int[] a = {1, 2, 3};
        //@ assert a.length == 3;
        //@ assert a[1] == 2;
      }
    }
  """

  vercors should verify using silicon in "example with literal java array value for field" java """
    class Test {
      int[] x = {1, 2, 3};

      Test() {
        //@ assert x.length == 3;
        //@ assert x[1] == 2;
      }
    }
  """

  vercors should verify using silicon in "example with explicit initialization of new array" java """
    class Test {
      void test() {
        int[] a = new int[] { 1, 2, 3 };
        //@ assert a.length == 3;
        //@ assert a[1] == 2;
      }
    }
  """
}
