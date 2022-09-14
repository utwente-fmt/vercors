package vct.test.integration.features

import vct.test.integration.helper.VercorsSpec

class PVLEOF extends VercorsSpec {
  vercors should verify using silicon in "example with line comment terminated by EOF" pvl """
    void test();
    // this is a comment
  """.strip()

  vercors should error withCode "parseError" in "example with garbage at the end" pvl """
    void test1();
    void test2();
    null
  """

  vercors should verify using silicon in "example with a declaration past a line comment" pvl """
    void test1() { test2(); }
    // this is a comment
    void test2();
  """
}
