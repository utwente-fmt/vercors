package integration.`new`

import vct.test.integration.helper.VercorsSpec

class ClassesSpec extends VercorsSpec {
  vercors should verify using anyBackend example "concepts/classes/DifferentClassesDifferentTypes1.java"
  vercors should verify using anyBackend example "concepts/classes/DifferentClassesDifferentTypes2.java"
  vercors should error withCode "parseError" example "concepts/classes/InnerClass.java"
  vercors should verify using anyBackend example "concepts/classes/Overloading.java"

}
