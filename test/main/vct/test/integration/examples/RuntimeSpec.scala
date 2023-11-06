package vct.test.integration.examples

import vct.test.integration.helper.VercorsSpec

import scala.language.postfixOps

class RuntimeSpec extends VercorsSpec {
  // Demos
  vercors should verify using runtime example "runtime/test.java" in 20

}
