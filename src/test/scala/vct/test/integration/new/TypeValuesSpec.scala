package integration.`new`

import vct.test.integration.helper.VercorsSpec

class TypeValuesSpec extends VercorsSpec {
  vercors should verify using anyBackend example "concepts/typevalues/cast-example.pvl"
  vercors should verify using anyBackend example "concepts/typevalues/CastExample.java"
  vercors should verify using anyBackend example "concepts/typevalues/TypeExample1.java"
}
