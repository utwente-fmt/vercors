package vct.test.integration.examples

import vct.test.integration.helper.VercorsSpec

class RefuteSpec extends VercorsSpec {
  vercors should verify using silicon example "concepts/refute/refute1.java"
  vercors should verify using silicon example "concepts/refute/refute2.java"
  vercors should verify using silicon example "concepts/refute/refute3.java"
  vercors should verify using silicon example "concepts/refute/refute4.java"
  vercors should verify using silicon example "concepts/refute/refute5.java"
}
