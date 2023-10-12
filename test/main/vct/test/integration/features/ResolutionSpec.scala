package vct.test.integration.features

import vct.test.integration.helper.VercorsSpec

class ResolutionSpec extends VercorsSpec {
  vercors should error withCode "noSuchName" in "example with incorrect usage from static context: single name invocation" java """
    class Test {
      //@ resource x() = true;

      //@ requires x();
      static void bar() {}
    }
  """

  vercors should error withCode "noSuchName" in "example with incorrect usage from static context: qualified invocation" java """
    class Test {
      //@ resource x() = true;

      //@ requires Test.x();
      static void bar() {}
    }
  """

  vercors should error withCode "noSuchName" in "example with incorrect usage from static context: single name" java """
    class Test {
      int x;

      //@ requires x == 5;
      static void bar() {}
    }
  """

  vercors should error withCode "noSuchName" in "example with incorrect usage from static context: qualified name" java """
    class Test {
      int x;

      //@ requires Test.x == 5;
      static void bar() {}
    }
  """
}
