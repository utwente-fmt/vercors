package integration.`new`

import integration.helper.VercorsSpec

class PermissionSpec extends VercorsSpec {
  vercors should verify using silicon example "concepts/permissions/BadLoop1.java"
  vercors should verify using silicon example "concepts/permissions/BadLoop2.java"
  vercors should verify using silicon example "concepts/permissions/box.pvl"
  vercors should verify using silicon example "concepts/permissions/Counter.java"
  vercors should verify using silicon example "concepts/permissions/IncrFail.java"
  vercors should verify using silicon example "concepts/permissions/IncrPass.java"
  vercors should verify using silicon example "concepts/permissions/MultiIncrement.java"
  vercors should verify using silicon example "concepts/permissions/RosterFixed.java"
  vercors should verify using silicon example "concepts/permissions/SwapIntegerFail.java"
  vercors should verify using silicon example "concepts/permissions/SwapIntegerPass.java"
  vercors should verify using silicon example "concepts/permissions/frame_error_1.pvl"
}
