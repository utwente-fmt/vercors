package integration.`new`

import integration.helper.VercorsSpec

class JavaImportSpec extends VercorsSpec {
  vercors should verify using anyBackend example "concepts/import/src/separate/Main.java"
  vercors should verify using anyBackend example "concepts/import/src/separate/Util.java"
}
