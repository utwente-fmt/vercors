package vct.test.integration.features

import vct.test.integration.helper.VercorsSpec

class OldSpec extends VercorsSpec {

  vercors should verify using anyBackend in "testing old val with label" pvl """

    class test {
      int x;

      requires Perm(x, write) ** x == 3;
      void old() {
        int loc = 4;
        x = x + 1;
        assert x == \old(x) + 1;
        loc = 3;
        assert \old(x == loc);
        x = x * 2;
        label test;
        loc = 8;
        x = 0;
        assert x != loc;
        assert \old[test](x == loc);
      }
    }


  """

}
