package vct.test.integration.examples

import vct.test.integration.helper.VercorsSpec

class TechnicalMinimizeSpec extends VercorsSpec {
  vercors should verify using silicon example "technical/minimize/FocusMethod.java"
  vercors should verify using silicon example "technical/minimize/FocusMethodTransitive.java"
  vercors should verify using silicon example "technical/minimize/FocusFunction.java"
  vercors should verify using silicon example "technical/minimize/FocusFunctionTransitive.java"
  vercors should verify using silicon example "technical/minimize/IgnoreFunction.java"
  vercors should verify using silicon example "technical/minimize/IgnoreMethod.java"

  vercors should verify using silicon example "technical/minimize/IgnoreConstructor.java"
  vercors should verify using silicon example "technical/minimize/IgnoreConstructor.pvl"
  vercors should verify using silicon example "technical/minimize/FocusConstructor.java"
  vercors should verify using silicon example "technical/minimize/FocusConstructor.pvl"

  vercors should verify using silicon example "technical/minimize/FocusTopLevelFunction.java"
  vercors should verify using silicon example "technical/minimize/FocusTopLevelFunction.pvl"
  vercors should verify using silicon example "technical/minimize/FocusTopLevelProcedure.pvl"
}
