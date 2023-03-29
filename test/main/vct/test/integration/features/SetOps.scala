package vct.test.integration.features

import vct.test.integration.helper.VercorsSpec

class SetOps extends VercorsSpec {
  vercors should verify using anyBackend in "example listing some set operators" pvl """
    void test() {
      set<int> b = {1, 2, 3};
      set<int> c = {3, 4};

      set<int> d = b - c; //false-positive type error
      // set<int> d = b.difference(c); //scala.MatchError

      assert d == {1, 2};
    }
  """
}
