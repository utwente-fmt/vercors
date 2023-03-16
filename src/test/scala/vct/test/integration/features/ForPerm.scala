package vct.test.integration.features

import vct.test.integration.helper.VercorsSpec

case class ForPerm() extends VercorsSpec {
  vercors should verify using anyBackend in "example showing usages of forperm" pvl """
    class Test {
      int f;

      requires Perm(f, write);
      void test1() {
        assert (\forperm Test t \in t.f; t == this);
      }

      requires Perm(f, write);
      requires \polarity_dependent((\forperm Test t \in t.f; t == this), (\forperm Test t \in t.f; false));
      void test2() {
      }

      requires Perm(f, write);
      void test2_use() {
        test2();
      }
    }
  """
}
