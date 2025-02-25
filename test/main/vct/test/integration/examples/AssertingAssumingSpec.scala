package vct.test.integration.examples

import vct.test.integration.helper.VercorsSpec

class AssertingAssumingSpec extends VercorsSpec {
  vercors should verify using silicon in
    "inline asserting should be verifiable" pvl """
    requires x > 2 && (asserting x > 2; true);
    void m(int x) { }
  """

  vercors should fail withCode "assertFailed:false" using silicon in
    "inline asserting should be rejectable" pvl """
    requires (asserting x > 2; true);
    void m(int x) { }
  """

  vercors should verify using silicon in
    "inline assuming should be verifiable" pvl """
    requires x > 2;
    ensures \result;
    pure boolean f(int x);

    requires (assuming x > 2; f(x));
    void m(int x) { }
  """

  vercors should fail withCode "preFailed:false" using silicon in
    "inline assuming should not cause unsoundness" pvl """
    requires x > 2;
    ensures \result;
    pure boolean f(int x);

    requires (assuming x > 1; f(x));
    void m(int x) { }
  """

  vercors should verify using silicon in
    "inline asserting should be verifiable (short form)" pvl """
    requires x > 2 && (asserting x > 2);
    void m(int x) { }
  """

  vercors should fail withCode "assertFailed:false" using silicon in
    "inline asserting should be rejectable (short form)" pvl """
    requires (asserting x > 2);
    void m(int x) { }
  """

  vercors should verify using silicon in
    "inline assuming should be verifiable (short form)" pvl """
    requires x > 2;
    ensures \result;
    pure boolean f(int x);

    requires (assuming x > 2) && f(x);
    void m(int x) { }
  """

  vercors should fail withCode "preFailed:false" using silicon in
    "inline assuming should not cause unsoundness (short form)" pvl """
    requires x > 2;
    ensures \result;
    pure boolean f(int x);

    requires (assuming x > 1) && f(x);
    void m(int x) { }
  """
}
