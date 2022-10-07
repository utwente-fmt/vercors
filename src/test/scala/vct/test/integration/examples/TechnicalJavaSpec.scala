package vct.test.integration.examples

import vct.test.integration.helper.VercorsSpec

class TechnicalJavaSpec extends VercorsSpec {
  vercors should verify using silicon example "technical/java/JavaAnnotation.java"
  vercors should verify using silicon example "technical/java/JavaString.java"
}
