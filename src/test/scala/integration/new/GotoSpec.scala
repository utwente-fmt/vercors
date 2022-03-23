package integration.`new`

import integration.helper.VercorsSpec

class GotoSpec extends VercorsSpec {
  vercors should verify using anyBackend example "concepts/goto/goto1.pvl"
  vercors should verify using anyBackend example "concepts/goto/goto2.pvl"
  vercors should verify using anyBackend example "concepts/goto/goto3.pvl"
  vercors should verify using anyBackend example "concepts/goto/LabeledIf.java"
  vercors should verify using anyBackend example "concepts/goto/LabeledWhile.java"
}
