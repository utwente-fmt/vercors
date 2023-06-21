package vct.test.integration.examples

import vct.test.integration.helper.VercorsSpec

class TechnicalPvlSpec extends VercorsSpec {
  vercors should verify using silicon in "ranged for loop" pvl
  """
  void m() {
    int max = -1;
    for (int i = 0 .. 10) {
      assert 0 <= i && i < 10;
      max = i;
    }
    assert 0 <= max && max <= 10;
  }
  """
}
