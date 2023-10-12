package vct.test.integration.features

import vct.test.integration.helper.VercorsSpec

class PatternInlinesLet extends VercorsSpec {
  vercors should verify using anyBackend in "example that shows let bindings are inlined in patterns" pvl """
    pure int f(int x);
    pure int g(int x, int y);

    requires (\forall int i; (\let int j = g(i, i); {: f(j) :} == 0));
    void test() {
      assert f(g(5, 5)) == 0;
    }
  """
}
