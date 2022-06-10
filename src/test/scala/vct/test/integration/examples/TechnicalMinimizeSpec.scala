package vct.test.integration.examples

import vct.test.integration.helper.VercorsSpec

class TechnicalMinimizeSpec extends VercorsSpec {
  vercors should verify using silicon example "technical/minimize/FocusMethod.java"
  vercors should verify using silicon example "technical/minimize/FocusMethodTransitive.java"
  vercors should verify using silicon example "technical/minimize/FocusFunction.java"
  vercors should verify using silicon example "technical/minimize/FocusFunctionTransitive.java"
}
