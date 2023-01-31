package vct.test.integration.features

import vct.test.integration.helper.VercorsSpec

class WhileSpec extends VercorsSpec {
  vercors should verify using anyBackend in "example showing information from side-effects in conditions is available in loops" pvl """
    class Test {
      int f;

      context Perm(f, write);
      ensures f == 5;
      ensures \result;
      boolean fIsFive();

      context Perm(f, write);
      void test() {
        loop_invariant Perm(f, write);
        while(fIsFive()) {
          assert f == 5;
        }
      }
    }
  """
}
