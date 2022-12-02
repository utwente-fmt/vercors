package vct.test.integration.examples

import org.scalatest.flatspec.AnyFlatSpecLike
import vct.test.integration.helper.VercorsSpec

class TechnicalJavaSpec extends VercorsSpec {
  vercors should verify using silicon example "technical/java/JavaAnnotation.java"
  vercors should verify using silicon example "technical/java/JavaString.java"

  vercors should verify using silicon in "java.lang classes should be reachable without imports" java
  """
  class C {
    int m() {
      //@ ghost System.staticInvariant();
      System.out.println("Hello world!");
      return Math.sqrt(2);
    }
  }
  """
}
