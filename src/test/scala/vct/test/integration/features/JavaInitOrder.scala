package vct.test.integration.features

import vct.test.integration.helper.VercorsSpec

class JavaInitOrder extends VercorsSpec {
  vercors should verify using anyBackend in "example with default field value" java """
    class Test {
      int x;
      //@ ensures Perm(x, write) ** x == 0;
      Test(){}
    }
  """

  vercors should verify using anyBackend in "example that changes the field" java """
    class Test {
      int x = 3;
      //@ ensures Perm(x, write) ** x == 3;
      Test(){}
    }
  """

  vercors should verify using anyBackend in "example that changes the field in the constructor" java """
    class Test {
      int x = 3;
      //@ ensures Perm(x, write) ** x == 4;
      Test() { x = 4; }
    }
  """
}
