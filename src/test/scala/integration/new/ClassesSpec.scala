package integration.`new`

import integration.helper.VercorsSpec

class ClassesSpec extends VercorsSpec {
  vercors should verify using anyBackend example "classes/DifferentClassesDifferentTypes1.java"
  vercors should verify using anyBackend example "classes/DifferentClassesDifferentTypes2.java"
  vercors should error withCode "parseError" example "classes/InnerClass.java"
  vercors should verify using anyBackend example "classes/Overloading.java"

}
