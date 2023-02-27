package vct.test.integration.examples

import vct.test.integration.helper.VercorsSpec

class JavaImportSpec extends VercorsSpec {
   vercors should verify using anyBackend example "concepts/import/src/separate/Main.java"
  vercors should verify using anyBackend example "concepts/import/src/separate/Util.java"
  vercors should verify using anyBackend example "concepts/import/Sqrt.java"
  vercors should verify using anyBackend example "concepts/import/SqrtStar.java"
}
