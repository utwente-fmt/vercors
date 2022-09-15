package vct.test.integration.features

import vct.test.integration.helper.VercorsSpec

class InlinePatterns extends VercorsSpec {
  vercors should verify using anyBackend in "inline pattern examples" pvl """
    pure int f(int i);
    pure int g(int i);

    void test1() {
      assume (∀int i; {:f(i):} == i);
      assert f(42) == 42;
    }

    void test2() {
      assume (∀int i; {:f(i):} == {:g(i):} && g(i) == 0);
      [/expect assertFailed:false] assert f(42) == 0; [/end]
    }

    void test3() {
      assume (∀int i; {:1:f(i):} == {:2:g(i):} && g(i) == 0);
      assert f(42) == 0;
    }

    void test4() {
      assume (∀int i; (∀int j; {:<:f(i):} + {:f(j):} == i + j));
      assert f(38) + f(39) == 77;
    }
  """
}
