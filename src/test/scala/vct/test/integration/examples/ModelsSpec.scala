package vct.test.integration.examples

import vct.test.integration.helper.VercorsSpec

class ModelsSpec extends VercorsSpec {
  vercors should verify using silicon example "concepts/futures/counteradd_2.pvl"
  vercors should verify using silicon example "concepts/futures/counteradd_n.pvl"
  vercors should verify using silicon example "concepts/futures/locking.pvl"
  vercors should verify using silicon example "concepts/futures/NoSendAfterRead.java"
  vercors should verify using silicon example "concepts/futures/TestFuture.pvl"
  vercors should verify using silicon example "concepts/futures/unequalcounting.pvl"
}
