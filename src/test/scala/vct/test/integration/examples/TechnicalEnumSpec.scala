package vct.test.integration.examples

import vct.test.integration.helper.VercorsSpec

class TechnicalEnumSpecX extends VercorsSpec {
  vercors should verify using silicon in "java/use builtin enums" java
    """
import java.lang.annotation.*;

class Test {
    void m() {
        RetentionPolicy r = RetentionPolicy.SOURCE;
    }
}

"""
}

class TechnicalEnumSpec extends VercorsSpec {
  vercors should verify using silicon examples("technical/enums/AB.java", "technical/enums/UseAB1.java")
  vercors should verify using silicon examples("technical/enums/AB.java", "technical/enums/UseAB2.java")
  vercors should verify using silicon example "technical/enums/Enums.java"

  vercors should verify using silicon in "java/use builtin enums" java
    """
      |import java.lang.annotation.RetentionPolicy;
      |
      |class Test {
      |    void m() {
      |        RetentionPolicy R = RetentionPolicy.SOURCE;
      |    }
      |}
      |""".stripMargin

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

    requires ab != null;
    void foo(AB ab) {
      if (ab != AB.A) {
        assert ab == AB.B;
      }
    }

    requires ab != null;
    void bar(AB ab) {
      assert ab == AB.A || ab == AB.B;
    }
  """

  vercors should verify using silicon in "pvl/enum seq" pvl """
    enum AB { A, B }

    requires |ab| > 0;
    requires (\forall int i = 0 .. |ab|; ab[i] != null);
    ensures (\forall int i, int j; 0 <= i && i < j && j < |\result|; \result[i] != \result[j]);
    seq<AB> foo(seq<AB> ab) {
      if (ab[0] != AB.A) {
        assert ab[0] == AB.B;
      }

      AB[] abs = new AB[1];
      abs[0] = ab[0];

      return seq<AB>{AB.A, AB.B};
    }
  """
}
