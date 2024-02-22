package vct.test.integration.examples

import vct.test.integration.helper.VercorsSpec

class GenericsExamplesSpec() extends VercorsSpec {
  vercors should verify using silicon example "concepts/generics/genericProcedure.pvl"
  vercors should verify using silicon example "concepts/generics/box.pvl"
  vercors should verify using silicon example "concepts/generics/genericChannel.pvl"
}
