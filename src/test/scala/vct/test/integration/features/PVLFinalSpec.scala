package vct.test.integration.features

import vct.test.integration.helper.VercorsSpec

class PVLFinalSpec extends VercorsSpec {
  vercors should verify using anyBackend in "example showing use of final in PVL" pvl """
    class Test {
      final int f;

      ensures this.f == f;
      constructor(int f) {
        this.f = f;
      }
    }

    void use() {
      Test t = new Test(5);
      assert t.f == 5;
    }
  """
}
