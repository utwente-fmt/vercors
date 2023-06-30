package vct.test.integration.examples

import vct.test.integration.helper.VercorsSpec

class TechnicalPvlSpec extends VercorsSpec {
  vercors should verify using silicon in "ranged for loop based on locals" pvl
  """
  void m() {
    int max = -1;
    loop_invariant max == i - 1;
    for (int i = 0 .. 10) {
      assert 0 <= i && i < 10;
      max = i;
    }
    assert max == 10 - 1;
  }
  """

  vercors should verify using silicon in "ranged for loop based on heap location" pvl
  """
  class C { int f; }

  requires Perm(c.f, 1) ** c.f > 0;
  void heapTest1(C c) {
      int max = -1;
      loop_invariant i - 1 == max;
      for (int i = 0 .. c.f) {
          max = i;
      }
      assert max == c.f - 1;
  }
  """

  vercors should verify using silicon in "ranged for loop based on heap location used inside" pvl
  """
  class C { int f; }

  requires Perm(c.f, 1) ** c.f > 0;
  void heapTest2(C c) {
    int max = -1;
    loop_invariant i - 1 == max;
    loop_invariant Perm(c.f, 1\2) ** c.f == \old(c.f);
    for (int i = 0 .. c.f) {
      assert 0 <= i && i < c.f;
      max = i;
    }
    assert max == c.f - 1;
  }
  """
}
