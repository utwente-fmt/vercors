package vct.test.integration.examples

import vct.test.integration.helper.VercorsSpec

class TechnicalEnumSpec extends VercorsSpec {
  vercors should verify using silicon in "pvl/enums" pvl """
    enum AB { A, B }
  """

  vercors should verify using silicon in "pvl/enum return" pvl """
    enum AB { A, B }

    AB foo() {
      return AB.A;
    }
  """

  vercors should verify using silicon in "pvl/enum inequality" pvl """
    enum AB { A, B }

    void foo() {
      assert AB.A != AB.B;
    }
  """

  vercors should verify using silicon in "pvl/enum exclusivity" pvl """
    enum AB { A, B }

    void foo(AB ab) {
      if (ab != AB.A) {
        assert ab == AB.B;
      }
    }

    void bar(AB ab) {
      assert ab == AB.A || ab == AB.B;
    }
  """

  vercors should verify using silicon in "pvl/enum seq" pvl """
    enum AB { A, B }

    requires |ab| > 0;
    ensures (\forall int i, int j; 0 <= i && i < j && j < |\result|; \result[i] != \result[j]);
    seq<AB> foo(seq<AB> ab) {
      if (ab[0] != AB.A) {
        assert ab[0] == AB.B;
      }

      return seq<AB>{AB.A, AB.B};
    }
  """
}
