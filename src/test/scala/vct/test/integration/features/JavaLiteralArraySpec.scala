package vct.test.integration.features

import vct.test.integration.helper.VercorsSpec

class JavaLiteralArraySpec extends VercorsSpec {
  vercors should verify using silicon in "example with literal java array value" java """
    class Test {
      void test() {
        int[] a = {1, 2, 3};
      }
    }
  """
}
